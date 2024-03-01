defmodule Ash.Reactor.CreateStep do
  @moduledoc """
  The Reactor step which is used to execute create actions.
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
      |> maybe_set_kw(:upsert_identity, options[:upsert_identity])
      |> maybe_set_kw(:upsert?, options[:upsert?])
      |> maybe_set_kw(:actor, arguments[:actor])
      |> maybe_set_kw(:tenant, arguments[:tenant])

    action_options =
      [return_notifications?: true]
      |> maybe_set_kw(:authorize?, options[:authorize?])

    options[:resource]
    |> Changeset.for_create(options[:action], arguments[:input], changeset_options)
    |> options[:api].create(action_options)
    |> case do
      {:ok, record} ->
        {:ok, record}

      {:ok, record, notifications} ->
        with :ok <- Ash.Reactor.Notifications.enqueue_notifications(context, notifications),
             do: {:ok, record}

      {:error, reason} ->
        {:error, reason}
    end
  end

  @doc false
  @impl true
  def undo(record, arguments, _context, options) do
    changeset_options =
      []
      |> maybe_set_kw(:authorize?, options[:authorize?])
      |> maybe_set_kw(:actor, arguments[:actor])
      |> maybe_set_kw(:tenant, arguments[:tenant])

    action_options =
      [return_notifications?: true, return_destroyed?: false]
      |> maybe_set_kw(:authorize?, options[:authorize?])

    record
    |> Changeset.for_destroy(options[:undo_action], arguments[:input], changeset_options)
    |> options[:api].destroy(action_options)
    # We always want to discard the notifications.
    |> case do
      :ok -> :ok
      {:ok, _} -> :ok
      {:ok, _, _} -> :ok
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
