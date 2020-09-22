defmodule Ash.Resource.Calculation.Builtins do
  @moduledoc "Built in calculations that are automatically imported in the calculations section"

  @doc "An example concatenation calculation, that accepts the delimeter as an argument"
  def concat(keys, separator \\ "") do
    {Ash.Resource.Calculation.Concat, keys: keys, separator: separator}
  end
end
