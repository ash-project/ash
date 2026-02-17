# SPDX-FileCopyrightText: 2025 crux contributors <https://github.com/ash-project/crux/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Crux.Expression.RewriteRule.ComplementLaw do
  @moduledoc """
  Rewrite rule that applies complement laws to simplify expressions.

  See: https://en.wikipedia.org/wiki/Boolean_algebra#Complement

  Applies the transformations:
  - `A OR NOT A = true` (law of excluded middle)
  - `NOT A OR A = true` (law of excluded middle)
  - `A AND NOT A = false` (law of contradiction)
  - `NOT A AND A = false` (law of contradiction)
  - `(A AND B) OR (A AND NOT B) = A` (complement distribution)
  - `(A OR B) AND (A OR NOT B) = A` (complement distribution)

  The complement laws handle expressions involving logical complements,
  detecting contradictions, tautologies, and simplifying distributive
  patterns with complements.
  """

  use Crux.Expression.RewriteRule

  import Crux.Expression, only: [b: 1]

  @impl Crux.Expression.RewriteRule
  def walk(b(expr or not expr)), do: true
  def walk(b(not expr or expr)), do: true
  def walk(b(expr and not expr)), do: false
  def walk(b(not expr and expr)), do: false

  def walk(b((expr and left) or (expr and not left))), do: expr
  def walk(b((left and expr) or (not left and expr))), do: expr

  def walk(b((expr or left) and (expr or not left))), do: expr
  def walk(b((left or expr) and (not left or expr))), do: expr

  def walk(other), do: other
end
