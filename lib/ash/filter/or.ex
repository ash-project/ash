defmodule Ash.Filter.Or do
  defstruct [:left, :right]

  def new([first | [last | []]]), do: {:ok, %__MODULE__{left: first, right: last}}
  def new([first | rest]), do: {:ok, %__MODULE__{left: first, right: new(rest)}}
  def new(left, right), do: {:ok, %__MODULE__{left: left, right: right}}
end
