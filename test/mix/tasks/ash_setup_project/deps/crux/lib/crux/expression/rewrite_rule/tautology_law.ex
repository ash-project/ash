# SPDX-FileCopyrightText: 2025 crux contributors <https://github.com/ash-project/crux/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Crux.Expression.RewriteRule.TautologyLaw do
  @moduledoc """
  Rewrite rule that detects common tautology patterns.

  Applies the transformations:
  - `A OR (NOT A OR B) = true` (tautology with additional terms)
  - `(NOT A OR B) OR A = true` (tautology with additional terms)
  - `(A OR B) OR NOT A = true` (tautology with additional terms)
  - `NOT A OR (A OR B) = true` (tautology with additional terms)

  These patterns represent tautologies where a complement pair appears
  in disjunction with additional terms, making the entire expression true.
  """

  use Crux.Expression.RewriteRule

  import Crux.Expression, only: [b: 1]

  @impl Crux.Expression.RewriteRule
  def walk(b(a or (not a or _b))), do: true
  def walk(b(a or (_b or not a))), do: true
  def walk(b(not a or _b or a)), do: true
  def walk(b(_b or not a or a)), do: true
  def walk(b(a or _b or not a)), do: true
  def walk(b(not a or (a or _b))), do: true
  def walk(b(not a or (_b or a))), do: true
  def walk(other), do: other
end
