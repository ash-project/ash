# SPDX-FileCopyrightText: 2020 Zach Daniel
#
# SPDX-License-Identifier: MIT

defmodule Ash.SatSolver.Expression.RewriteRule.AssociativityLaw do
  @moduledoc false

  # Rewrite rule that applies associativity optimizations to simplify expressions.
  #
  # See: https://en.wikipedia.org/wiki/Associative_property
  #
  # Applies the transformations:
  # - `A OR (A OR B) = A OR B`
  # - `A OR B OR A = A OR B`
  # - `A AND (A AND B) = A AND B`
  # - `A AND B AND A = A AND B`
  #
  # The associativity optimizations leverage the associative property of boolean
  # operations to eliminate redundant terms when the same expression appears
  # multiple times in an associative context.

  use Ash.SatSolver.Expression.RewriteRule

  import Ash.SatSolver.Expression, only: [b: 1]

  @impl Ash.SatSolver.Expression.RewriteRule
  def walk(b(expr or (expr or other))), do: b(expr or other)
  def walk(b(expr or other or expr)), do: b(expr or other)
  def walk(b(expr and (expr and other))), do: b(expr and other)
  def walk(b(expr and other and expr)), do: b(expr and other)
  def walk(other), do: other
end
