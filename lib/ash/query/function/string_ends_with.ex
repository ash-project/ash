# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Query.Function.StringEndsWith do
  @moduledoc """
  Returns true if the first string ends with the second.

  Case insensitive strings are accounted for on either side.

     string_ends_with("foo", "oo")
     true

     string_ends_with(%Ash.CiString{string: "foo"}, "OO")
     true

     string_ends_with("foo", %Ash.CiString{string: "OO"})
     true
  """
  use Ash.Query.Function, name: :string_ends_with, predicate?: true

  alias Ash.CiString

  def args,
    do: [
      [:string, :string],
      [:string, :ci_string],
      [:ci_string, :string],
      [:ci_string, :ci_string]
    ]

  def returns, do: [:boolean, :boolean, :boolean, :boolean]

  def evaluate(%{arguments: [nil, _]}), do: {:known, false}
  def evaluate(%{arguments: [_, nil]}), do: {:known, false}

  def evaluate(%{arguments: [%CiString{} = left, %CiString{} = right]}) do
    {:known,
     String.ends_with?(
       CiString.to_comparable_string(left),
       CiString.to_comparable_string(right)
     )}
  end

  def evaluate(%{arguments: [left, %CiString{} = right]}) when is_binary(left) do
    {:known,
     left
     |> CiString.to_comparable_string()
     |> String.ends_with?(CiString.to_comparable_string(right))}
  end

  def evaluate(%{arguments: [%CiString{} = left, right]}) when is_binary(right) do
    {:known,
     String.ends_with?(
       CiString.to_comparable_string(left),
       CiString.to_comparable_string(right)
     )}
  end

  def evaluate(%{arguments: [left, right]}) when is_binary(left) and is_binary(right) do
    {:known, String.ends_with?(left, right)}
  end

  def evaluate(_other) do
    :unknown
  end

  def can_return_nil?(%{arguments: arguments}) do
    Enum.any?(arguments, &Ash.Expr.can_return_nil?/1)
  end
end
