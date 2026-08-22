# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Query.Function.RangeAdjacent do
  @moduledoc """
  Returns true if two ranges are adjacent, meeting with no gap and no shared point.

  The seam counts only when exactly one side includes it. Symmetric, and an empty
  range is adjacent to nothing.

     range_adjacent(range1, range2)
  """
  use Ash.Query.Function, name: :range_adjacent, predicate?: true

  alias Ash.Range

  def args, do: [[:any, :same]]

  def returns, do: [:boolean]

  def evaluate(%{arguments: [nil, _]}), do: {:known, nil}
  def evaluate(%{arguments: [_, nil]}), do: {:known, nil}

  def evaluate(%{arguments: [%Range{} = left, %Range{} = right]}) do
    {:known, Range.adjacent?(left, right)}
  end

  def evaluate(_other), do: :unknown

  def can_return_nil?(%{arguments: arguments}) do
    Enum.any?(arguments, &Ash.Expr.can_return_nil?/1)
  end
end
