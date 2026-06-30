# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Query.Function.RangeOverlaps do
  @moduledoc """
  Returns true if two ranges overlap (share at least one point).

  Maps to the Postgres range overlap operator `&&`. Used, among other things, to
  relate two temporal resources (`range_overlaps(parent(valid_at), valid_at)`).

     range_overlaps(range1, range2)
  """
  use Ash.Query.Function, name: :range_overlaps, predicate?: true

  alias Ash.Range

  def args, do: [[:any, :same]]

  def returns, do: [:boolean]

  def evaluate(%{arguments: [nil, _]}), do: {:known, nil}
  def evaluate(%{arguments: [_, nil]}), do: {:known, nil}

  def evaluate(%{arguments: [%Range{} = left, %Range{} = right]}) do
    {:known, do_overlap?(left, right)}
  end

  def evaluate(_other), do: :unknown

  def can_return_nil?(%{arguments: arguments}) do
    Enum.any?(arguments, &Ash.Expr.can_return_nil?/1)
  end

  # Two ranges overlap iff neither lies entirely on one side of the other, accounting for
  # each bound's inclusivity (`[`/`]` inclusive, `(`/`)` exclusive) and treating a `nil`
  # bound as ±∞. Empty ranges (e.g. `[5, 5)`) overlap nothing. Matches Postgres `&&`.
  defp do_overlap?(left, right) do
    not empty?(left) and not empty?(right) and
      not separated?(left, right) and not separated?(right, left)
  end

  # `x` lies entirely at/below `y`, sharing no point at the `x.upper` / `y.lower` seam.
  defp separated?(%Range{upper: nil}, _y), do: false
  defp separated?(_x, %Range{lower: nil}), do: false

  defp separated?(%Range{upper: xu, bounds: xb}, %Range{lower: yl, bounds: yb}) do
    cond do
      Comp.less_than?(xu, yl) -> true
      # Touching boundary: a shared point only if BOTH sides include it.
      Comp.equal?(xu, yl) -> not (upper_inclusive?(xb) and lower_inclusive?(yb))
      true -> false
    end
  end

  # A bounded range with no points: `lower > upper`, or `lower == upper` unless both bounds
  # are inclusive (`[x, x]`, the single point `x`). Unbounded ends are never empty.
  defp empty?(%Range{lower: l, upper: u, bounds: b}) when not is_nil(l) and not is_nil(u) do
    cond do
      Comp.less_than?(u, l) -> true
      Comp.equal?(l, u) -> not (lower_inclusive?(b) and upper_inclusive?(b))
      true -> false
    end
  end

  defp empty?(_range), do: false

  defp lower_inclusive?(bounds), do: bounds in [:"[)", :"[]"]
  defp upper_inclusive?(bounds), do: bounds in [:"(]", :"[]"]
end
