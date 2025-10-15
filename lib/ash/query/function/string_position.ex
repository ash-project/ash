# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Query.Function.StringPosition do
  @moduledoc """
  Returns the zero-based position of a substring within a string, nil if the string does not contain the substring.

  Case insensitive strings are accounted for on either side.

     string_position("foo", "fo")
     0

     string_position(%Ash.CiString{string: "foo"}, "FoO")
     0

     string_position("foo", %Ash.CiString{string: "FOO"})
     0
  """
  use Ash.Query.Function, name: :string_position

  alias Ash.CiString

  def args,
    do: [
      [:string, :string],
      [:string, :ci_string],
      [:ci_string, :string],
      [:ci_string, :ci_string]
    ]

  def returns, do: [:string, :ci_string, :ci_string, :ci_string]

  def evaluate(%{arguments: [nil, _]}), do: {:known, nil}
  def evaluate(%{arguments: [_, nil]}), do: {:known, nil}

  def evaluate(%{arguments: [left, right]})
      when is_struct(left, CiString) or is_struct(right, CiString) do
    left_comparable =
      if is_struct(left, CiString),
        do: CiString.to_comparable_string(left),
        else: CiString.to_comparable_string(%CiString{string: left})

    right_comparable =
      if is_struct(right, CiString),
        do: CiString.to_comparable_string(right),
        else: CiString.to_comparable_string(%CiString{string: right})

    {:known, position(left_comparable, right_comparable)}
  end

  def evaluate(%{arguments: [left, right]}) when is_binary(left) and is_binary(right) do
    {:known, position(left, right)}
  end

  def evaluate(_other) do
    :unknown
  end

  defp position(string, substring) do
    case String.split(string, substring, parts: 2) do
      [before, _after] -> String.length(before)
      _ -> nil
    end
  end
end
