# SPDX-FileCopyrightText: ash contributors <https://github.com/ash-project/ash/graphs.contributors>
#
#
# SPDX-License-Identifier: MIT

defmodule Ash.SatSolver.Expression.RewriteRule.TautologyLaw do
  @moduledoc false

  # Rewrite rule that detects common tautology patterns.
  #
  # Applies the transformations:
  # - `A OR (NOT A OR B) = true` (tautology with additional terms)
  # - `(NOT A OR B) OR A = true` (tautology with additional terms)
  # - `(A OR B) OR NOT A = true` (tautology with additional terms)
  # - `NOT A OR (A OR B) = true` (tautology with additional terms)
  #
  # These patterns represent tautologies where a complement pair appears
  # in disjunction with additional terms, making the entire expression true.

  use Ash.SatSolver.Expression.RewriteRule

  import Ash.SatSolver.Expression, only: [b: 1]

  @impl Ash.SatSolver.Expression.RewriteRule
  def walk(b(a or (not a or _b))), do: true
  def walk(b(a or (_b or not a))), do: true
  def walk(b(not a or _b or a)), do: true
  def walk(b(_b or not a or a)), do: true
  def walk(b(a or _b or not a)), do: true
  def walk(b(not a or (a or _b))), do: true
  def walk(b(not a or (_b or a))), do: true
  def walk(other), do: other
end
