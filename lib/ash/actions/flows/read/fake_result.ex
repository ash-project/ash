defmodule Ash.Actions.Flows.Read.FakeResult do
  @moduledoc """
  Generates a fake result, as the flow has to actually return something.
  """
  use Ash.Flow.Step

  def run(_input, _opts, _context) do
    {:ok, []}
  end
end
