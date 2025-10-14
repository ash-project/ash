# SPDX-FileCopyrightText: 2020 Zach Daniel
#
# SPDX-License-Identifier: MIT

defmodule Ash.SatSolver.Expression.RewriteRule.ComplementLaw do
  @moduledoc false

  # Rewrite rule that applies complement laws to simplify expressions.
  #
  # See: https://en.wikipedia.org/wiki/Boolean_algebra#Complement
  #
  # Applies the transformations:
  # - `A OR NOT A = true` (law of excluded middle)
  # - `NOT A OR A = true` (law of excluded middle)
  # - `A AND NOT A = false` (law of contradiction)
  # - `NOT A AND A = false` (law of contradiction)
  # - `(A AND B) OR (A AND NOT B) = A` (complement distribution)
  # - `(A OR B) AND (A OR NOT B) = A` (complement distribution)
  #
  # The complement laws handle expressions involving logical complements,
  # detecting contradictions, tautologies, and simplifying distributive
  # patterns with complements.

  use Ash.SatSolver.Expression.RewriteRule

  import Ash.SatSolver.Expression, only: [b: 1]

  @impl Ash.SatSolver.Expression.RewriteRule
  def walk(b(expr or not expr)), do: true
  def walk(b(not expr or expr)), do: true
  def walk(b(expr and not expr)), do: false
  def walk(b(not expr and expr)), do: false
  def walk(b((expr and left) or (expr and not left))), do: expr
  def walk(b((expr or left) and (expr or not left))), do: expr
  def walk(other), do: other
end
