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
end
