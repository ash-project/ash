# SPDX-FileCopyrightText: 2020 Zach Daniel
#
# SPDX-License-Identifier: MIT

defmodule Ash.SatSolver.Expression.RewriteRule.DistributivityBasedSimplificationLaw do
  @moduledoc false

  # Rewrite rule that applies distributivity-based simplifications to expressions.
  #
  # See: https://en.wikipedia.org/wiki/Distributive_property
  #
  # Applies the transformations:
  # - `A AND (NOT A OR B) = A AND B`
  # - `A OR (NOT A AND B) = A OR B`
  # - `NOT A AND (A OR B) = NOT A AND B`
  # - `NOT A OR (A AND B) = NOT A OR B`
  #
  # These patterns use distributivity properties to eliminate redundant terms
  # involving complements, simplifying expressions by removing parts that
  # don't affect the overall result.

  use Ash.SatSolver.Expression.RewriteRule

  import Ash.SatSolver.Expression, only: [b: 1]

  @impl Ash.SatSolver.Expression.RewriteRule
  def walk(b(expr and (not expr or right))), do: b(expr and right)
  def walk(b(expr or (not expr and right))), do: b(expr or right)
  def walk(b(not expr and (expr or right))), do: b(not expr and right)
  def walk(b(not expr or (expr and right))), do: b(not expr or right)
  def walk(other), do: other
end
