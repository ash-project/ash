defmodule Ash.Reactor.ReadOneStep do
  @moduledoc """
  The Reactor step which is used to execute get actions.
  """
  use Reactor.Step
  import Ash.Reactor.StepUtils

  alias Ash.Query

  def run(arguments, context, options) do
    query_options =
      options
      |> maybe_set_kw(:authorize?, options[:authorize?])
      |> maybe_set_kw(:actor, arguments[:actor])
      |> maybe_set_kw(:tenant, arguments[:tenant])
      |> maybe_set_kw(:authorize?, context[:authorize?])
      |> maybe_set_kw(:actor, context[:actor])
      |> maybe_set_kw(:tenant, context[:tenant])
      |> maybe_set_kw(:tracer, context[:tracer])

    action_options =
      []
      |> maybe_set_kw(:authorize?, options[:authorize?])

    options[:resource]
    |> Query.for_read(options[:action], arguments[:input], query_options)
    |> options[:domain].read_one(action_options)
    |> case do
      {:ok, nil} ->
        if options[:fail_on_not_found?] do
          raise Ash.Error.Query.NotFound, resource: options[:resource]
        else
          {:ok, nil}
        end

      {:ok, record} ->
        {:ok, record}

      {:ok, records, _} ->
        {:ok, records}

      {:error, reason} ->
        {:error, reason}
    end
  end
end
