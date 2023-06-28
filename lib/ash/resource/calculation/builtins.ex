defmodule Ash.Resource.Calculation.Builtins do
  @moduledoc "Built in calculations that are automatically imported in the calculations section"

  @doc """
  An example concatenation calculation, that accepts the delimiter as an argument

  ## Examples

      calculate :full_name, :string, concat([:first_name, :last_name], " ")
  """
  @spec concat(keys :: list(atom), separator :: String.t()) :: Ash.Resource.Calculation.ref()
  def concat(keys, separator \\ "") do
    {Ash.Resource.Calculation.Concat, keys: keys, separator: separator}
  end
end
