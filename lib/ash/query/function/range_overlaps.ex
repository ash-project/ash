# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Query.Function.RangeOverlaps do
  @moduledoc """
  Returns true if two ranges overlap (share at least one point).

  Maps to the Postgres range overlap operator `&&`, and answers in an expression what
  `Ash.Range.intersects?/2` answers at runtime. Used, among other things, to relate
  two temporal resources (`range_overlaps(parent(valid_at), valid_at)`).

     range_overlaps(range1, range2)
  """
  use Ash.Query.Function, name: :range_overlaps, predicate?: true

  alias Ash.Range

  def args, do: [[:any, :same]]

  def returns, do: [:boolean]

  def evaluate(%{arguments: [nil, _]}), do: {:known, nil}
  def evaluate(%{arguments: [_, nil]}), do: {:known, nil}

  # Overlap is a fact about two ranges, so `Ash.Range` answers it. Postgres `&&` is
  # intersection, which is what `intersects?/2` is named for.
  def evaluate(%{arguments: [%Range{} = left, %Range{} = right]}) do
    {:known, Range.intersects?(left, right)}
  end

  def evaluate(_other), do: :unknown

  def can_return_nil?(%{arguments: arguments}) do
    Enum.any?(arguments, &Ash.Expr.can_return_nil?/1)
  end
end
