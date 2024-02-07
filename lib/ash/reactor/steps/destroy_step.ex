defmodule Ash.Reactor.DestroyStep do
  @moduledoc """
  The Reactor step which is used to execute update actions.
  """
  use Reactor.Step
  import Ash.Reactor.StepUtils

  alias Ash.Changeset

  def run(arguments, _context, options) do
    changeset_options =
      options
      |> maybe_set_kw(:authorize?, options[:authorize?])
      |> maybe_set_kw(:actor, arguments[:actor])
      |> maybe_set_kw(:tenant, arguments[:tenant])
      |> maybe_set_kw(:return_destroyed?, options[:return_destroyed?])

    action_options =
      []
      |> maybe_set_kw(:authorize?, options[:authorize?])
      |> maybe_set_kw(:return_destroyed?, options[:return_destroyed?])

    arguments[:initial]
    |> Changeset.for_destroy(options[:action], arguments[:input], changeset_options)
    |> options[:api].destroy(action_options)
    |> case do
      :ok ->
        {:ok, :ok}

      {:ok, record} when is_struct(record) ->
        {:ok, record}

      {:ok, notifications} when is_list(notifications) ->
        # FIXME do something with the notifications
        {:ok, :ok}

      {:ok, record, _notifications} ->
        # FIXME do something with the notifications
        {:ok, record}

      {:error, reason} ->
        {:error, reason}
    end
  end
end
