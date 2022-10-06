defmodule Ash.Test.Flow.Steps.Error do
  @moduledoc false
  use Ash.Flow.Step

  def run(%{error: :raise}, _opts, _context) do
    raise "uh oh!"
  end

  def run(%{error: :return}, _opts, _context) do
    {:error, "uh oh!"}
  end

  def run(_input, _opts, _context) do
    {:ok, nil}
  end
end
