# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Notifier do
  @moduledoc """
  A notifier is an extension that receives various events
  """
  @callback notify(Ash.Notifier.Notification.t()) :: :ok
  @callback requires_original_data?(Ash.Resource.t(), Ash.Resource.Actions.action()) :: boolean

  require Ash.Tracer

  defmacro __using__(_) do
    quote do
      @behaviour Ash.Notifier

      def requires_original_data?(_, _), do: false

      defoverridable requires_original_data?: 2
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
      case notification.for do
        nil ->
          for notifier <- Ash.Resource.Info.notifiers(resource) do
            do_notify(notifier, %{notification | from: self()})
          end

        allowed_notifiers ->
          for notifier <- Enum.uniq(List.wrap(allowed_notifiers)) do
            do_notify(notifier, %{notification | from: self()})
          end
      end
    end

    unsent
    |> Enum.map(&elem(&1, 1))
    |> List.flatten()
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
