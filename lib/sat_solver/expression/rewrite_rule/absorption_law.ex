# SPDX-FileCopyrightText: 2020 Zach Daniel
#
# SPDX-License-Identifier: MIT

defmodule Ash.SatSolver.Expression.RewriteRule.AbsorptionLaw do
  @moduledoc false

  # Rewrite rule that applies absorption laws to simplify expressions.
  #
  # See: https://en.wikipedia.org/wiki/Absorption_law
  #
  # Applies the transformations:
  # - `A AND (A OR B) = A`
  # - `(A OR B) AND A = A`
  # - `A OR (A AND B) = A`
  # - `(A AND B) OR A = A`
  #
  # The absorption laws state that a term can absorb another term when
  # one is a logical subset of the other.

  use Ash.SatSolver.Expression.RewriteRule

  import Ash.SatSolver.Expression, only: [b: 1]

  @impl Ash.SatSolver.Expression.RewriteRule
  def walk(b(expr and (expr or _other))), do: expr
  def walk(b((expr or _other) and expr)), do: expr
  def walk(b(expr or (expr and _other))), do: expr
  def walk(b((expr and _other) or expr)), do: expr
  def walk(other), do: other
end
