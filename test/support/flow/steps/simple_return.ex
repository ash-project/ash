defmodule Ash.Test.Flow.Steps.SimpleReturn do
  @moduledoc false
  use Ash.Flow.Step

  def run(%{return: return}, _opts, _context) do
    {:ok, return}
  end
end
