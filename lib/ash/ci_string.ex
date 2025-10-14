# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.CiString do
  @moduledoc """
  Represents a case insensitive string

  While some data layers are aware of case insensitive string types, in order for values
  of this type to be used in other parts of Ash Framework, it has to be embedded in a module
  this allows us to implement the `Comparable` protocol for it.

  For the type implementation, see `Ash.Type.CiString`
  """

  defstruct [:string, casted?: false, case: nil]

  @type casing :: nil | :lower | :upper

  @type t :: %__MODULE__{
          string: String.t(),
          casted?: boolean(),
          case: casing()
        }
  @type string_type :: t() | String.t()

  @doc "Creates a case insensitive string"
  @spec sigil_i(string_type() | nil, charlist()) :: t()
  def sigil_i(value, mods) do
    cond do
      ?l in mods ->
        new(value, :lower)

      ?u in mods ->
        new(value, :upper)

      true ->
        new(value)
    end
  end

  defimpl Jason.Encoder do
    def encode(ci_string, opts) do
      ci_string
      |> Ash.CiString.value()
      |> Jason.Encode.string(opts)
    end
  end

  defimpl String.Chars do
    def to_string(ci_string) do
      Ash.CiString.value(ci_string)
    end
  end

  defimpl Inspect do
    import Inspect.Algebra

    def inspect(%Ash.CiString{string: string}, opts) do
      concat(["#Ash.CiString<", to_doc(string, opts), ">"])
    end
  end

  @doc """
  Returns a Ash.CiString from a String, or `nil` if the value is `nil`.
  """
  @spec new(string_type() | nil, casing()) :: t() | nil
  def new(value, casing \\ nil)

  def new(nil, _), do: nil

  def new(%__MODULE__{string: value}, casing) do
    new(value, casing)
  end

  def new(value, casing) do
    case casing do
      :upper ->
        %Ash.CiString{casted?: true, string: value && String.upcase(value)}

      :lower ->
        %Ash.CiString{casted?: true, string: value && String.downcase(value)}

      nil ->
        %Ash.CiString{casted?: false, string: value}
    end
  end

  @doc """
  Converts a `Ash.CiString` into a normal Elixir String.
  """
  @spec value(t()) :: String.t()
  def value(%Ash.CiString{string: value, casted?: false, case: :lower}) do
    value && String.downcase(value)
  end

  def value(%Ash.CiString{string: value, casted?: false, case: :upper}) do
    value && String.upcase(value)
  end

  def value(%Ash.CiString{string: value}) do
    value
  end

  @doc """
  Compares an Elixir String or Ash.CiString. It will return `:eq` if equal, `:lt`, if
  the string is ordered alphabetically before the second string, and `:gt` if after.
  """
  @spec compare(string_type(), string_type()) :: :gt | :lt | :eq
  def compare(left, right) do
    do_compare(to_comparable_string(left), to_comparable_string(right))
  end

  defp do_compare(left, right) when left == right, do: :eq
  defp do_compare(left, right) when left < right, do: :lt
  defp do_compare(_, _), do: :gt

  @doc "Returns the downcased value, only downcasing if it hasn't already been done"
  @spec to_comparable_string(string_type() | nil) :: String.t() | nil
  def to_comparable_string(value) when is_binary(value) do
    String.downcase(value)
  end

  def to_comparable_string(%__MODULE__{case: :lower, casted?: true, string: value}) do
    value
  end

  def to_comparable_string(%__MODULE__{string: value}) do
    value && String.downcase(value)
  end

  def to_comparable_string(nil), do: nil
end

import Ash.Type.Comparable

defcomparable left :: Ash.CiString, right :: BitString do
  Ash.CiString.compare(left, right)
end

defcomparable left :: Ash.CiString, right :: Ash.CiString do
  Ash.CiString.compare(left, right)
end
