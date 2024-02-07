defmodule Ash.Reactor.UpdateStep do
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

    arguments[:initial]
    |> Changeset.for_update(options[:action], arguments[:input], changeset_options)
    |> options[:api].update()
  end
end
