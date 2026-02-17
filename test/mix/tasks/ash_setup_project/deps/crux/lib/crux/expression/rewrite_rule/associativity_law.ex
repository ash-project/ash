# SPDX-FileCopyrightText: 2025 crux contributors <https://github.com/ash-project/crux/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Crux.Expression.RewriteRule.AssociativityLaw do
  @moduledoc """
  Rewrite rule that applies associativity optimizations to simplify expressions.

  See: https://en.wikipedia.org/wiki/Associative_property

  Applies the transformations:
  - `A OR (A OR B) = A OR B`
  - `A OR B OR A = A OR B`
  - `A AND (A AND B) = A AND B`
  - `A AND B AND A = A AND B`

  The associativity optimizations leverage the associative property of boolean
  operations to eliminate redundant terms when the same expression appears
  multiple times in an associative context.
  """

  use Crux.Expression.RewriteRule

  import Crux.Expression, only: [b: 1]

  @impl Crux.Expression.RewriteRule
  def walk(b(expr or (expr or other))), do: b(expr or other)
  def walk(b(expr or (other or expr))), do: b(expr or other)

  def walk(b(expr or other or expr)), do: b(expr or other)
  def walk(b(other or expr or expr)), do: b(expr or other)

  def walk(b(expr and (expr and other))), do: b(expr and other)
  def walk(b(expr and (other and expr))), do: b(expr and other)

  def walk(b(expr and other and expr)), do: b(expr and other)
  def walk(b(other and expr and expr)), do: b(expr and other)

  def walk(other), do: other
end
