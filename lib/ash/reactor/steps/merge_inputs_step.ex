defmodule Ash.Reactor.MergeInputsStep do
  @moduledoc """
  A custom step which merges any number of `inputs` results into a single map.
  """
  use Reactor.Step

  @doc false
  @impl true
  def run(arguments, _context, _options) do
    result =
      arguments
      |> Map.values()
      |> Enum.reduce(&Map.merge/2)

    {:ok, result}
  end
end
