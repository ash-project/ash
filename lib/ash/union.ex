defmodule Ash.Union do
  @type t :: %__MODULE__{}
  defstruct [:value, :type]

  defimpl Jason.Encoder do
    def encode(%{value: value}, opts) do
      Jason.Encode.value(value, opts)
    end
  end
end
