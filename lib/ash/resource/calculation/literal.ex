defmodule Ash.Resource.Calculation.Literal do
  @moduledoc false
  use Ash.Resource.Calculation

  def calculate(records, opts, _context) do
    if opts[:precomputed?] do
      opts[:value]
    else
      Enum.map(records, fn _ -> opts[:value] end)
    end
  end

  def expression(_records, opts, _context) do
    opts[:value]
  end
end
