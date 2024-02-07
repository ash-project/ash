defmodule Ash.Reactor.CreateStep do
  @moduledoc """
  The Reactor step which is used to execute create actions.
  """
  use Reactor.Step
  import Ash.Reactor.StepUtils

  alias Ash.Changeset

  def run(arguments, _context, options) do
    changeset_options =
      options
      |> maybe_set_kw(:authorize?, options[:authorize?])
      |> maybe_set_kw(:upsert_identity, options[:upsert_identity])
      |> maybe_set_kw(:upsert?, options[:upsert?])
      |> maybe_set_kw(:actor, arguments[:actor])
      |> maybe_set_kw(:tenant, arguments[:tenant])

    options[:resource]
    |> Changeset.for_create(options[:action], arguments[:input], changeset_options)
    |> options[:api].create()
    |> case do
      {:ok, record} ->
        {:ok, record}

      {:ok, record, _notifications} ->
        # FIXME do something with the notifications
        {:ok, record}

      {:error, reason} ->
        {:error, reason}
    end
  end
end
