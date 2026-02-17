# SPDX-FileCopyrightText: 2025 crux contributors <https://github.com/ash-project/crux/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Crux.Expression.RewriteRule.CommutativityLaw do
  @moduledoc """
  Rewrite rule that applies commutativity laws to normalize expression order.

  See: https://en.wikipedia.org/wiki/Commutative_property

  Applies the transformations:
  - `A AND B = B AND A`
  - `A OR B = B OR A`

  This normalization helps other rewrite rules match patterns more effectively
  by ensuring a consistent lexicographic order of operands.
  """

  use Crux.Expression.RewriteRule

  import Crux.Expression, only: [b: 1]

  @impl Crux.Expression.RewriteRule
  def walk(b(left and right)) do
    [sorted_left, sorted_right] = Enum.sort([left, right])
    b(sorted_left and sorted_right)
  end

  def walk(b(left or right)) do
    [sorted_left, sorted_right] = Enum.sort([left, right])
    b(sorted_left or sorted_right)
  end

  def walk(other), do: other
end
