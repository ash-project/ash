defmodule Ash.Reactor.ActionStep do
  @moduledoc """
  The Reactor step which is used to execute generic actions.
  """
  use Reactor.Step
  import Ash.Reactor.StepUtils

  alias Ash.ActionInput

  def run(arguments, _context, options) do
    action_input_options =
      options
      |> maybe_set_kw(:authorize?, options[:authorize?])
      |> maybe_set_kw(:actor, arguments[:actor])
      |> maybe_set_kw(:tenant, arguments[:tenant])

    action_options =
      []
      |> maybe_set_kw(:authorize?, options[:authorize?])

    options[:resource]
    |> ActionInput.for_action(options[:action], arguments[:input], action_input_options)
    |> options[:api].run_action(action_options)
    |> case do
      {:ok, record} ->
        {:ok, record}

      {:error, reason} ->
        {:error, reason}
    end
  end
end
