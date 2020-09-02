defmodule Ash.Resource.Calculation.Builtins do
  @moduledoc "Built in calculations that are automatically imported in the calculations section"
  def concat(keys, separator \\ "") do
    {Ash.Resource.Calculation.Concat, keys: keys, separator: separator}
  end
end
