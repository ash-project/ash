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
      |> maybe_set_kw(:authorize?, options[:authorize?])
      |> maybe_set_kw(:actor, arguments[:actor])
      |> maybe_set_kw(:tenant, arguments[:tenant])

    action_options =
      [return_notifications?: true]
      |> maybe_set_kw(:authorize?, options[:authorize?])

    changeset =
      arguments[:initial]
      |> Changeset.for_update(options[:action], arguments[:input], changeset_options)

    changeset
    |> options[:api].update(action_options)
    |> case do
      {:ok, record} ->
        {:ok, set_metadata(record, context.current_step.name, :changeset, changeset)}

      {:ok, record, notifications} ->
        with :ok <- Ash.Reactor.Notifications.enqueue_notifications(context, notifications),
             do: {:ok, set_metadata(record, context.current_step.name, :changeset, changeset)}

      {:error, reason} ->
        {:error, reason}
    end
  end

  @doc false
  @impl true
  def undo(record, arguments, context, options) do
    changeset_options =
      options
      |> maybe_set_kw(:authorize?, options[:authorize?])
      |> maybe_set_kw(:actor, arguments[:actor])
      |> maybe_set_kw(:tenant, arguments[:tenant])

    action_options =
      [return_notifications?: false]
      |> maybe_set_kw(:authorize?, options[:authorize?])

    attributes =
      %{changeset: get_metadata(record, context.current_step.name, :changeset)}

    record
    |> Changeset.for_update(options[:undo_action], attributes, changeset_options)
    |> options[:api].update(action_options)
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

  defp set_metadata(record, step_name, key, value) do
    Ash.Resource.set_metadata(record, %{__reactor__: %{step_name => %{key => value}}})
  end

  defp get_metadata(record, step_name, key) do
    Ash.Resource.get_metadata(record, [:__reactor__, step_name, key])
  end
end
