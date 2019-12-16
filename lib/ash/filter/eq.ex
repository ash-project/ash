defmodule Ash.Filter.Eq do
  defstruct [:value]

  def new(value), do: {:ok, %__MODULE__{value: value}}
end
