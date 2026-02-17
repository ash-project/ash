# SPDX-FileCopyrightText: 2025 crux contributors <https://github.com/ash-project/crux/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Crux.Expression.RewriteRule.NegationLaw do
  @moduledoc """
  Rewrite rule that applies negation laws to simplify expressions.

  See: https://en.wikipedia.org/wiki/Negation

  Applies the transformations:
  - `NOT true = false`
  - `NOT false = true`
  - `NOT (NOT A) = A` (double negation elimination)

  The negation laws handle boolean constant negation and double negation
  elimination, providing a complete set of negation simplifications.
  """

  use Crux.Expression.RewriteRule

  import Crux.Expression, only: [b: 1]

  @impl Crux.Expression.RewriteRule
  def walk(b(not true)), do: false
  def walk(b(not false)), do: true
  def walk(b(not not expr)), do: expr
  def walk(other), do: other
end
