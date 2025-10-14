# SPDX-FileCopyrightText: ash contributors <https://github.com/ash-project/ash/graphs.contributors>
#
#
# SPDX-License-Identifier: MIT

defmodule Ash.SatSolver.Expression.RewriteRule.NegationLaw do
  @moduledoc false

  # Rewrite rule that applies negation laws to simplify expressions.
  #
  # See: https://en.wikipedia.org/wiki/Negation
  #
  # Applies the transformations:
  # - `NOT true = false`
  # - `NOT false = true`
  # - `NOT (NOT A) = A` (double negation elimination)
  #
  # The negation laws handle boolean constant negation and double negation
  # elimination, providing a complete set of negation simplifications.

  use Ash.SatSolver.Expression.RewriteRule

  import Ash.SatSolver.Expression, only: [b: 1]

  @impl Ash.SatSolver.Expression.RewriteRule
  def walk(b(not true)), do: false
  def walk(b(not false)), do: true
  def walk(b(not not expr)), do: expr
  def walk(other), do: other
end
