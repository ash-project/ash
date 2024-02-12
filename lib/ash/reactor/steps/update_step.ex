defmodule Ash.Reactor.UpdateStep do
  @moduledoc """
  The Reactor step which is used to execute update actions.
  """
  use Reactor.Step
  import Ash.Reactor.StepUtils

  alias Ash.Changeset

  def run(arguments, context, options) do
    changeset_options =
      options
      |> maybe_set_kw(:authorize?, options[:authorize?])
      |> maybe_set_kw(:actor, arguments[:actor])
      |> maybe_set_kw(:tenant, arguments[:tenant])

    action_options =
      [return_notifications?: true]
      |> maybe_set_kw(:authorize?, options[:authorize?])

    arguments[:initial]
    |> Changeset.for_update(options[:action], arguments[:input], changeset_options)
    |> options[:api].update(action_options)
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
end
