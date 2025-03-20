defmodule Ash.Reactor.GetStep do
  @moduledoc """
  The Reactor step which is used to execute get actions.
  """
  use Reactor.Step
  import Ash.Reactor.StepUtils

  alias Ash.Query
  require Query

  def run(arguments, context, options) do
    query_options =
      options
      |> maybe_set_kw(:tracer, context[:tracer])
      |> maybe_set_kw(:authorize?, context[:authorize?])
      |> maybe_set_kw(:actor, context[:actor])
      |> maybe_set_kw(:tenant, context[:tenant])
      |> maybe_set_kw(:authorize?, options[:authorize?])
      |> maybe_set_kw(:actor, arguments[:actor])
      |> maybe_set_kw(:tenant, arguments[:tenant])

    action_options =
      [domain: options[:domain]]
      |> maybe_set_kw(:authorize?, options[:authorize?])
      |> maybe_set_kw(:load, arguments[:load])
      |> maybe_set_kw(:not_found_error?, options[:fail_on_not_found?])
      |> maybe_set_kw(:context, arguments[:context])

    filter_keys = Keyword.fetch!(options, :by)

    {filters, input} =
      arguments[:input]
      |> Map.split(filter_keys)

    options[:resource]
    |> Query.for_read(options[:action], input, query_options)
    |> Query.filter_input(filters)
    |> Ash.read_one(action_options)
  end
end
