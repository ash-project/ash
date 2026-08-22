# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Range do
  @moduledoc """
  A continuous range of values of some inner type, with inclusive/exclusive bounds.

  The value representation for `Ash.Type.Range`. `bounds` follows Postgres range
  notation: the first character is the lower bound, the second the upper —
  `[` / `]` inclusive, `(` / `)` exclusive. A `nil` `lower`/`upper` is an
  unbounded (infinite) end. The default `:"[)"` (lower-inclusive, upper-exclusive)
  is the convention that lets adjacent ranges tile a timeline without overlap.

  A range containing no points is empty, and every empty range is the same range.
  `Ash.Type.Range` casts any such range to `empty/0`, whose bounds are dropped —
  as Postgres does — so that empty ranges compare equal and survive storage in a
  data layer that keeps no bounds for them.
  """

  @type bounds :: :"[)" | :"[]" | :"()" | :"(]"

  @type t :: %__MODULE__{
          lower: term() | nil,
          upper: term() | nil,
          bounds: bounds(),
          empty?: boolean()
        }

  defstruct lower: nil, upper: nil, bounds: :"[)", empty?: false

  @valid_bounds [:"[)", :"[]", :"()", :"(]"]

  @doc "Whether the given atom is a valid bounds specifier."
  @spec valid_bounds?(term()) :: boolean()
  def valid_bounds?(bounds), do: bounds in @valid_bounds

  @doc "The empty range: the one range containing no points."
  @spec empty() :: t()
  def empty, do: %__MODULE__{lower: nil, upper: nil, bounds: :"[)", empty?: true}

  @doc """
  Whether the range contains no points.

  True for `empty/0`, and for a bounded range whose bounds admit nothing: a lower
  above its upper, or bounds that meet without both including the point they meet
  at. An unbounded end is never empty.
  """
  @spec empty?(t()) :: boolean()
  def empty?(%__MODULE__{empty?: true}), do: true

  def empty?(%__MODULE__{lower: lower, upper: upper, bounds: bounds})
      when not is_nil(lower) and not is_nil(upper) do
    cond do
      Comp.less_than?(upper, lower) -> true
      Comp.equal?(lower, upper) -> not (lower_inclusive?(bounds) and upper_inclusive?(bounds))
      true -> false
    end
  end

  def empty?(%__MODULE__{}), do: false

  @doc "Whether the range's lower bound includes the point it names."
  @spec lower_inclusive?(bounds()) :: boolean()
  def lower_inclusive?(bounds), do: bounds in [:"[)", :"[]"]

  @doc "Whether the range's upper bound includes the point it names."
  @spec upper_inclusive?(bounds()) :: boolean()
  def upper_inclusive?(bounds), do: bounds in [:"(]", :"[]"]

  @doc """
  Whether the range holds `value`, which may be a point or another range.

  An unbounded end holds everything beyond it, and an empty range holds no point.
  Each bound is compared with `Comp`, so an inner type behaves inside a range as
  it does outside one, and a bound that excludes its own value (`(` or `)`) is
  not held.

  A range holds another when the second lies within the first, sharing an endpoint
  or being equal included. Every range holds the empty range, as Postgres `@>` does.
  """
  @spec contains?(t(), term()) :: boolean()
  def contains?(%__MODULE__{} = range, %__MODULE__{} = value) do
    empty?(value) or
      (not empty?(range) and
         relation(range, value) in [:contains, :started_by, :finished_by, :equals])
  end

  def contains?(%__MODULE__{} = range, value) do
    not empty?(range) and above_lower?(range, value) and below_upper?(range, value)
  end

  defp above_lower?(%{lower: nil}, _value), do: true

  defp above_lower?(range, value) do
    case Comp.compare(value, range.lower) do
      :gt -> true
      :eq -> lower_inclusive?(range.bounds)
      :lt -> false
    end
  end

  defp below_upper?(%{upper: nil}, _value), do: true

  defp below_upper?(range, value) do
    case Comp.compare(value, range.upper) do
      :lt -> true
      :eq -> upper_inclusive?(range.bounds)
      :gt -> false
    end
  end

  @doc """
  Whether two ranges share any point.

  An empty range intersects nothing, not even itself. Each range must start at or
  before the other ends, and a boundary the two ranges share counts only when both
  sides include it — so `[1,3)` and `[3,5)` do not intersect, where `[1,3]` and
  `[3,5)` do. Bounds are compared with `Comp`, as everywhere else here.

  Named for what it answers rather than for the operator it backs. Postgres calls
  `&&` "overlap" and `range_overlaps/2` keeps that name, but Allen's *overlaps* is
  the narrower relation where two ranges cross with neither containing the other —
  under which `[1,10)` and `[3,5)` do **not** overlap. This returns true for them.
  """
  @spec intersects?(t(), t()) :: boolean()
  def intersects?(%__MODULE__{} = left, %__MODULE__{} = right) do
    not empty?(left) and not empty?(right) and
      starts_before_end?(left, right) and starts_before_end?(right, left)
  end

  defp starts_before_end?(%{lower: nil}, _other), do: true
  defp starts_before_end?(_range, %{upper: nil}), do: true

  defp starts_before_end?(range, other) do
    case Comp.compare(range.lower, other.upper) do
      :lt -> true
      :eq -> lower_inclusive?(range.bounds) and upper_inclusive?(other.bounds)
      :gt -> false
    end
  end

  @doc """
  Compares two ranges as Postgres orders them: empty first, then by lower bound, then
  by upper, an unbounded end as `-∞`/`+∞`, and the earlier boundary first where two
  bounds name the same value (`[1` before `(1`, `5)` before `5]`).

  A sort order rather than containment: `[1,10)` sorting before `[3,5)` says nothing
  about one holding the other.
  """
  @spec compare(t(), t()) :: :lt | :eq | :gt
  def compare(%__MODULE__{} = left, %__MODULE__{} = right) do
    case {empty?(left), empty?(right)} do
      {true, true} -> :eq
      {true, false} -> :lt
      {false, true} -> :gt
      {false, false} -> compare_bounds(left, right)
    end
  end

  defp compare_bounds(left, right) do
    with :eq <- compare_lower(left, right) do
      compare_upper(left, right)
    end
  end

  defp compare_lower(%{lower: nil}, %{lower: nil}), do: :eq
  defp compare_lower(%{lower: nil}, _right), do: :lt
  defp compare_lower(_left, %{lower: nil}), do: :gt

  defp compare_lower(left, right) do
    with :eq <- Comp.compare(left.lower, right.lower) do
      earlier(lower_inclusive?(left.bounds), lower_inclusive?(right.bounds))
    end
  end

  defp compare_upper(%{upper: nil}, %{upper: nil}), do: :eq
  defp compare_upper(%{upper: nil}, _right), do: :gt
  defp compare_upper(_left, %{upper: nil}), do: :lt

  defp compare_upper(left, right) do
    with :eq <- Comp.compare(left.upper, right.upper) do
      earlier(not upper_inclusive?(left.bounds), not upper_inclusive?(right.bounds))
    end
  end

  @allen [
    :precedes,
    :meets,
    :overlaps,
    :finished_by,
    :contains,
    :starts,
    :equals,
    :started_by,
    :during,
    :finishes,
    :overlapped_by,
    :met_by,
    :preceded_by
  ]

  @typedoc "One of Allen's thirteen interval relations."
  @type allen :: unquote(Enum.reduce(Enum.reverse(@allen), &{:|, [], [&1, &2]}))

  @doc """
  Every relation `relation/2` can answer, in Allen's canonical order.

  Sorted by how far `left` begins before `right`, then by how far it ends before, with
  `equals` at the centre and each relation the converse of its mirror. A set of
  relations is conventionally a thirteen-bit mask, so these are the bit positions.
  """
  @spec relations() :: [allen()]
  def relations, do: @allen

  @doc """
  Which of Allen's thirteen relations `left` bears to `right`, or `nil` if either is empty.

  Exactly one holds for any pair of non-empty ranges, so the answer classifies rather
  than tests. An empty range has no relation to anything: it precedes nothing and is
  during nothing, having no points to be positioned by.

  Two ranges *meet* when one ends where the other begins and exactly one of them
  includes that point — `[1,5)` meets `[5,9)`, where `[1,5]` overlaps it and `(5,9)`
  merely follows it. Matches Postgres `-|-`.
  """
  @spec relation(t(), t()) :: allen() | nil
  def relation(%__MODULE__{} = left, %__MODULE__{} = right) do
    if empty?(left) or empty?(right) do
      nil
    else
      allen(
        compare_lower(left, right),
        compare_upper(left, right),
        seam(left, right),
        seam(right, left)
      )
    end
  end

  @doc """
  Whether two ranges are adjacent: one ends exactly where the other begins, with no
  point between them and none shared.

  The seam counts only when exactly one side includes it, so `[1,5)` is adjacent to
  `[5,9)`, where `[1,5]` overlaps it and `(5,9)` leaves a gap. Symmetric, unlike
  Allen's *meets*, which is directional. An empty range is adjacent to nothing.

  Adjacency is what lets a series of ranges tile: each meets the next, covering
  everything between the first lower bound and the last upper without overlapping.

  For a discrete inner type this is a question about the canonical form — `[1,4]` and
  `[5,9)` are adjacent as integers but not as decimals — so compare values that have
  been through `Ash.Type.Range`, which canonicalises them. Matches Postgres `-|-`.
  """
  @spec adjacent?(t(), t()) :: boolean()
  def adjacent?(%__MODULE__{} = left, %__MODULE__{} = right) do
    relation(left, right) in [:meets, :met_by]
  end

  # Seam clauses first, holding whatever the bounds compare to; then canonical order.
  defp allen(_lower, _upper, :lt, _), do: :precedes
  defp allen(_lower, _upper, :eq, _), do: :meets
  defp allen(_lower, _upper, _, :eq), do: :met_by
  defp allen(_lower, _upper, _, :lt), do: :preceded_by
  defp allen(:lt, :lt, _, _), do: :overlaps
  defp allen(:lt, :eq, _, _), do: :finished_by
  defp allen(:lt, :gt, _, _), do: :contains
  defp allen(:eq, :lt, _, _), do: :starts
  defp allen(:eq, :eq, _, _), do: :equals
  defp allen(:eq, :gt, _, _), do: :started_by
  defp allen(:gt, :lt, _, _), do: :during
  defp allen(:gt, :eq, _, _), do: :finishes
  defp allen(:gt, :gt, _, _), do: :overlapped_by

  # Where `left` ends against where `right` begins: `:lt` a gap, `:eq` meets, `:gt` shares
  # a point. An unbounded end reaches past every bound, so it always shares.
  defp seam(%__MODULE__{upper: nil}, _right), do: :gt
  defp seam(_left, %__MODULE__{lower: nil}), do: :gt

  defp seam(left, right) do
    cond do
      Comp.less_than?(left.upper, right.lower) -> :lt
      Comp.greater_than?(left.upper, right.lower) -> :gt
      upper_inclusive?(left.bounds) and lower_inclusive?(right.bounds) -> :gt
      upper_inclusive?(left.bounds) or lower_inclusive?(right.bounds) -> :eq
      true -> :lt
    end
  end

  # The boundary sitting earlier on the line sorts first.
  defp earlier(same, same), do: :eq
  defp earlier(true, false), do: :lt
  defp earlier(false, true), do: :gt
end

import Ash.Type.Comparable

defcomparable left :: Ash.Range, right :: Ash.Range do
  Ash.Range.compare(left, right)
end
