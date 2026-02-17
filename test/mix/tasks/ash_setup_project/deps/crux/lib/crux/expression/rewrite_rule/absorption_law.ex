# SPDX-FileCopyrightText: 2025 crux contributors <https://github.com/ash-project/crux/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Crux.Expression.RewriteRule.AbsorptionLaw do
  @moduledoc """
  Rewrite rule that applies absorption laws to simplify expressions.

  See: https://en.wikipedia.org/wiki/Absorption_law

  Applies the transformations:
  - `A AND (A OR B) = A`
  - `(A OR B) AND A = A`
  - `A OR (A AND B) = A`
  - `(A AND B) OR A = A`

  The absorption laws state that a term can absorb another term when
  one is a logical subset of the other.
  """

  use Crux.Expression.RewriteRule

  import Crux.Expression, only: [b: 1]

  @impl Crux.Expression.RewriteRule
  def walk(b(expr and (expr or _other))), do: expr
  def walk(b(expr and (_other or expr))), do: expr

  def walk(b((expr or _other) and expr)), do: expr
  def walk(b((_other or expr) and expr)), do: expr

  def walk(b(expr or (expr and _other))), do: expr
  def walk(b(expr or (_other and expr))), do: expr

  def walk(b((expr and _other) or expr)), do: expr
  def walk(b((_other and expr) or expr)), do: expr

  def walk(other), do: other
end
