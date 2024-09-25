defmodule Ash.Reactor.LoadStep do
  @moduledoc """
  The Reactor step which is used to execute load steps.
  """
  use Reactor.Step
  import Ash.Reactor.StepUtils

  def run(arguments, context, options) do
    load_options =
      options
      |> maybe_set_kw(:authorize?, context[:authorize?])
      |> maybe_set_kw(:actor, context[:actor])
      |> maybe_set_kw(:tenant, context[:tenant])
      |> maybe_set_kw(:tracer, context[:tracer])
      |> maybe_set_kw(:actor, arguments[:actor])
      |> maybe_set_kw(:tenant, arguments[:tenant])
      |> maybe_set_kw(:context, arguments[:context])

    arguments.records
    |> Ash.load(arguments.load, load_options)
  end
end
