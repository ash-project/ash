defmodule Ash.Filter.In do
  defstruct [:values]

  def new([]), do: {:ok, %Ash.Filter.Impossible{cause: :empty_in}}
  def new(values), do: {:ok, %__MODULE__{values: List.wrap(values)}}
end
