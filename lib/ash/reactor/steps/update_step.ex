defmodule Ash.Reactor.UpdateStep do
  @moduledoc """
  The Reactor step which is used to execute update actions.
  """
  use Reactor.Step
  import Ash.Reactor.StepUtils

  alias Ash.{Changeset, DataLayer}

  @doc false
  @impl true
  def run(arguments, context, options) do
    changeset_options =
      options
      |> maybe_set_kw(:authorize?, context[:authorize?])
      |> maybe_set_kw(:actor, context[:actor])
      |> maybe_set_kw(:tenant, context[:tenant])
      |> maybe_set_kw(:tracer, context[:tracer])
      |> maybe_set_kw(:authorize?, options[:authorize?])
      |> maybe_set_kw(:actor, arguments[:actor])
      |> maybe_set_kw(:tenant, arguments[:tenant])

    action_options =
      [return_notifications?: true, domain: options[:domain]]
      |> maybe_set_kw(:authorize?, options[:authorize?])
      |> maybe_set_kw(:load, arguments[:load])
      |> maybe_set_kw(:context, arguments[:context])

    changeset =
      arguments[:initial]
      |> Changeset.for_update(options[:action], arguments[:input], changeset_options)

    changeset
    |> Ash.update(action_options)
    |> case do
      {:ok, record} ->
        {:ok, store_changeset_in_metadata(context.current_step.name, record, changeset)}

      {:ok, record, notifications} ->
        with :ok <- Ash.Reactor.Notifications.enqueue_notifications(context, notifications),
             do: {:ok, store_changeset_in_metadata(context.current_step.name, record, changeset)}

      {:error, reason} ->
        {:error, reason}
    end
  end

  @doc false
  @impl true
  def undo(record, arguments, context, options) do
    changeset_options =
      options
      |> maybe_set_kw(:authorize?, context[:authorize?])
      |> maybe_set_kw(:actor, context[:actor])
      |> maybe_set_kw(:tenant, context[:tenant])
      |> maybe_set_kw(:tracer, context[:tracer])
      |> maybe_set_kw(:authorize?, options[:authorize?])
      |> maybe_set_kw(:actor, arguments[:actor])
      |> maybe_set_kw(:tenant, arguments[:tenant])

    action_options =
      [return_notifications?: false, domain: options[:domain]]
      |> maybe_set_kw(:authorize?, options[:authorize?])

    attributes =
      %{changeset: get_changeset_from_metadata(context.current_step.name, record)}

    record
    |> Changeset.for_update(options[:undo_action], attributes, changeset_options)
    |> Ash.update(action_options)
    |> case do
      {:ok, _record} -> :ok
      {:ok, _record, _notifications} -> :ok
      {:error, reason} -> {:error, reason}
    end
  end

  @doc false
  @impl true
  def can?(%{impl: {_, options}}, :undo) do
    case options[:undo] do
      :always -> true
      :never -> false
      :outside_transaction -> !DataLayer.in_transaction?(options[:resource])
    end
  end

  def can?(_, :compensate), do: false

  @doc false
  @impl true
  def async?(%{impl: {_, options}, async?: async}) do
    cond do
      DataLayer.in_transaction?(options[:resource]) ->
        false

      is_function(async, 1) ->
        async.(options)

      true ->
        async
    end
  end
end
