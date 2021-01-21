defmodule Ash.CiString do
  @moduledoc """
  Represents a case insensitive string

  While some data layers are aware of case insensitive string types, in order for values
  of this type to be used in other parts of Ash Framework, it has to be embedded in a module
  this allows us to implement the `Comparable` protocol for it.

  For the type implementation, see `Ash.Type.CiString`
  """

  defstruct [:string, lowered?: false]

  def sigil_i(value), do: %Ash.CiString{lowered?: true, string: String.downcase(value)}

  defimpl Jason.Encoder do
    def encode(%Ash.CiString{string: string, lowered?: false}, _opts) do
      String.downcase(string)
    end

    def encode(%Ash.CiString{string: string, lowered?: true}, _opts) do
      string
    end
  end

  defimpl String.Chars do
    def to_string(%Ash.CiString{lowered?: false, string: string}), do: String.downcase(string)
    def to_string(%Ash.CiString{string: string}), do: string
  end

  def compare(left, right) do
    do_compare(to_binary(left), to_binary(right))
  end

  defp do_compare(left, right) when left < right, do: :lt
  defp do_compare(left, right) when left == right, do: :eq
  defp do_compare(_, _), do: :gt

  defp to_binary(value) when is_binary(value) do
    String.downcase(value)
  end

  defp to_binary(%__MODULE__{lowered?: false, string: value}) do
    String.downcase(value)
  end

  defp to_binary(%__MODULE__{string: value}) do
    value
  end
end

use Comp

defcomparable left :: Ash.CiString, right :: String do
  Ash.CiString.compare(left, right)
end

defcomparable left :: Ash.CiString, right :: Ash.CiString do
  Ash.CiString.compare(left, right)
end
