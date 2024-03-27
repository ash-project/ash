defmodule Ash.Reactor.DestroyStep do
  @moduledoc """
  The Reactor step which is used to execute update actions.
  """
  use Reactor.Step
  import Ash.Reactor.StepUtils

  alias Ash.{Changeset, DataLayer}

  @doc false
  @impl true
  def run(arguments, context, options) do
    return_destroyed? = can?(context.current_step, :undo) || options[:return_destroyed?]

    changeset_options =
      options
      |> maybe_set_kw(:authorize?, options[:authorize?])
      |> maybe_set_kw(:actor, arguments[:actor])
      |> maybe_set_kw(:tenant, arguments[:tenant])
      |> maybe_set_kw(:return_destroyed?, return_destroyed?)

    action_options =
      [return_notifications?: true]
      |> maybe_set_kw(:authorize?, options[:authorize?])
      |> maybe_set_kw(:return_destroyed?, return_destroyed?)

    arguments[:initial]
    |> Changeset.for_destroy(options[:action], arguments[:input], changeset_options)
    |> options[:domain].destroy(action_options)
    |> case do
      :ok ->
        {:ok, :ok}

      {:ok, record} when is_struct(record) ->
        maybe_return_destroyed?(record, return_destroyed?)

      {:ok, notifications} when is_list(notifications) ->
        with :ok <- Ash.Reactor.Notifications.enqueue_notifications(context, notifications),
             do: {:ok, :ok}

      {:ok, record, notifications} ->
        with :ok <- Ash.Reactor.Notifications.enqueue_notifications(context, notifications),
             do: maybe_return_destroyed?(record, return_destroyed?)

      {:error, reason} ->
        {:error, reason}
    end
  end

  @doc false
  @impl true
  def undo(record, arguments, _context, options) do
    changeset_options =
      options
      |> maybe_set_kw(:authorize?, options[:authorize?])
      |> maybe_set_kw(:actor, arguments[:actor])
      |> maybe_set_kw(:tenant, arguments[:tenant])

    action_options =
      [return_notifications?: false]
      |> maybe_set_kw(:authorize?, options[:authorize?])

    options[:resource]
    |> Changeset.for_create(options[:undo_action], %{record: record}, changeset_options)
    |> options[:domain].create(action_options)
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

  defp maybe_return_destroyed?(record, true), do: {:ok, record}
  defp maybe_return_destroyed?(_record, false), do: {:ok, :ok}
end
