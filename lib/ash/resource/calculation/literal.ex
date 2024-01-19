defmodule Ash.Resource.Calculation.Literal do
  @moduledoc false
  use Ash.Calculation

  def calculate(records, opts, _context) do
    Enum.map(records, fn _ -> opts[:value] end)
  end

  def expression(_records, opts, _context) do
    opts[:value]
  end
end
