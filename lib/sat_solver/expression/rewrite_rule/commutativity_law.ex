# SPDX-FileCopyrightText: 2020 Zach Daniel
#
# SPDX-License-Identifier: MIT

defmodule Ash.SatSolver.Expression.RewriteRule.CommutativityLaw do
  @moduledoc false

  # Rewrite rule that applies commutativity laws to normalize expression order.
  #
  # See: https://en.wikipedia.org/wiki/Commutative_property
  #
  # Applies the transformations:
  # - `A AND B = B AND A`
  # - `A OR B = B OR A`
  #
  # This normalization helps other rewrite rules match patterns more effectively
  # by ensuring a consistent lexicographic order of operands.

  use Ash.SatSolver.Expression.RewriteRule

  import Ash.SatSolver.Expression, only: [b: 1]

  @impl Ash.SatSolver.Expression.RewriteRule
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
