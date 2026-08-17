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
  """

  @type bounds :: :"[)" | :"[]" | :"()" | :"(]"

  @type t :: %__MODULE__{
          lower: term() | nil,
          upper: term() | nil,
          bounds: bounds()
        }

  defstruct lower: nil, upper: nil, bounds: :"[)"

  @valid_bounds [:"[)", :"[]", :"()", :"(]"]

  @doc "Whether the given atom is a valid bounds specifier."
  @spec valid_bounds?(term()) :: boolean()
  def valid_bounds?(bounds), do: bounds in @valid_bounds
end
