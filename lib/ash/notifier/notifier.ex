# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Notifier do
  @moduledoc """
  A notifier is an extension that receives various events
  """
  @callback notify(Ash.Notifier.Notification.t()) :: :ok
  @callback requires_original_data?(Ash.Resource.t(), Ash.Resource.Actions.action()) :: boolean

  @doc """
  A load statement to be applied before this notifier receives the notification.

  The loaded fields are merged onto `notification.data` before `notify/1` is called.
  If multiple notifiers request the same fields with the same arguments, the
  calculation dependency resolver ensures they are only loaded once.

  The return value can be anything accepted by `Ash.Query.load/2`, including
  `%Ash.Query.Calculation{}` structs for multi-level dependency resolution
  (e.g. a PubSub notifier building one inner load per publication).
  """
  @callback load(Ash.Resource.t(), Ash.Resource.Actions.action()) ::
              atom | [atom] | Keyword.t()

  @optional_callbacks load: 2

  require Ash.Tracer

  defmacro __using__(_opts) do
    quote do
      @behaviour Ash.Notifier

      def requires_original_data?(_, _), do: false
      def load(_, _), do: []

      defoverridable requires_original_data?: 2, load: 2
    end
  end

  @doc """
  Sends any notifications that can be sent, and returns the rest.

  A notification can only be sent if you are not currently in a transaction
  for the resource in question.
  """
  @spec notify(list(Ash.Notifier.Notification.t()) | Ash.Notifier.Notification.t()) ::
          list(Ash.Notifier.Notification.t())
  def notify([]), do: []

  def notify(resource_notifications) do
    {unsent, to_send} =
      resource_notifications
      |> List.wrap()
      |> Enum.group_by(& &1.resource)
      |> Enum.split_with(fn {resource, _} ->
        resource && Ash.DataLayer.in_transaction?(resource)
      end)

    for {resource, notifications} <- to_send, notification <- notifications do
      notifiers =
        case notification.for do
          nil -> Ash.Resource.Info.notifiers(resource)
          allowed_notifiers -> Enum.uniq(List.wrap(allowed_notifiers))
        end

      {notification, notifier_data} = load_notification_data(notification, notifiers)

      for notifier <- notifiers do
        enriched = enrich_notification(notification, notifier, notifier_data)
        do_notify(notifier, %{enriched | from: self()})
      end
    end

    unsent
    |> Enum.map(&elem(&1, 1))
    |> List.flatten()
  end

  # Builds one NotifierDependencies calculation per notifier that has a
  # non-empty load statement, then does a single Ash.load so the calculation
  # dependency resolver can deduplicate identical underlying loads.
  defp load_notification_data(notification, notifiers) do
    notifier_statements =
      Enum.reduce(notifiers, %{}, fn notifier, acc ->
        statement =
          if function_exported?(notifier, :load, 2) do
            notifier.load(notification.resource, notification.action)
          else
            []
          end

        case List.wrap(statement) do
          [] -> acc
          statement -> Map.put(acc, notifier, statement)
        end
      end)

    if map_size(notifier_statements) == 0 do
      {notification, %{}}
    else
      domain =
        notification.domain ||
          Ash.Resource.Info.domain(notification.resource)

      source_context =
        (notification.changeset && notification.changeset.context) || %{}

      query =
        Enum.reduce(
          notifier_statements,
          Ash.Query.new(notification.resource),
          fn {notifier, statement}, query ->
            Ash.Query.calculate(
              query,
              {Ash.Notifier.NotifierDependencies, notifier: notifier},
              Ash.Type.Map,
              {Ash.Notifier.NotifierDependencies, [statement: statement, notifier: notifier]},
              %{},
              [],
              %{},
              source_context: source_context
            )
          end
        )

      case Ash.load(
             notification.data,
             query,
             domain: domain,
             authorize?: false,
             context: %{private: %{internal?: true}}
           ) do
        {:ok, loaded_data} ->
          notifier_data =
            Map.new(notifier_statements, fn {notifier, statement} ->
              extra =
                Map.get(
                  loaded_data.calculations || %{},
                  {Ash.Notifier.NotifierDependencies, notifier: notifier},
                  %{}
                )

              {notifier, {statement, extra}}
            end)

          {%{notification | data: loaded_data}, notifier_data}

        {:error, _error} ->
          {notification, %{}}
      end
    end
  end

  # Merges the notifier's dependency map onto notification.data.
  # Placement is determined by the original load statement intent:
  # attributes, relationships, and aggregates go directly on the struct;
  # calculations without a `load` field go into .calculations.
  defp enrich_notification(notification, notifier, notifier_data) do
    case Map.get(notifier_data, notifier) do
      nil ->
        notification

      {_statement, extra} when map_size(extra) == 0 ->
        notification

      {statement, extra} ->
        resource = notification.resource

        enriched_data =
          Enum.reduce(extra, notification.data, fn {key, value}, data ->
            case placement_for_key(key, statement, resource) do
              :struct -> Map.put(data, key, value)
              :calculations -> Map.update!(data, :calculations, &Map.put(&1 || %{}, key, value))
            end
          end)

        %{notification | data: enriched_data}
    end
  end

  # Determines whether a key from the notifier's extra map belongs on the
  # resource struct directly or in record.calculations, based on resource schema
  # and any %Ash.Query.Calculation{} structs in the original statement.
  @doc false
  def placement_for_key(key, statement, resource) do
    calc_struct =
      Enum.find(List.wrap(statement), fn
        %Ash.Query.Calculation{name: ^key} -> true
        _ -> false
      end)

    case calc_struct do
      %Ash.Query.Calculation{load: load} when not is_nil(load) ->
        :struct

      %Ash.Query.Calculation{} ->
        :calculations

      nil ->
        cond do
          Ash.Resource.Info.attribute(resource, key) ->
            :struct

          Ash.Resource.Info.relationship(resource, key) ->
            :struct

          Ash.Resource.Info.aggregate(resource, key) ->
            :struct

          Ash.Resource.Info.calculation(resource, key) ->
            :calculations

          true ->
            :calculations
        end
    end
  end

  defp do_notify(notifier, notification) do
    tracer = notification.changeset && notification.changeset.context[:private][:tracer]
    domain = notification.domain || Ash.Resource.Info.domain(notification.resource)

    Ash.Tracer.span :notification,
                    fn -> Ash.Domain.Info.span_name(domain, notification.resource, :notifier) end,
                    tracer do
      metadata = fn ->
        %{
          domain: domain,
          notifier: notifier,
          resource: notification.resource,
          resource_short_name: Ash.Resource.Info.short_name(notification.resource),
          actor: notification.changeset && notification.changeset.context[:private][:actor],
          tenant: notification.changeset && notification.changeset.context[:private][:tenant],
          action: notification.action.name,
          authorize?:
            notification.changeset && notification.changeset.context[:private][:authorize?]
        }
      end

      Ash.Tracer.set_metadata(tracer, :action, metadata)

      Ash.Tracer.telemetry_span [:ash, :notifier], metadata do
        notifier.notify(notification)
      end
    end
  end
end
