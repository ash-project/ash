# SPDX-FileCopyrightText: 2025 crux contributors <https://github.com/ash-project/crux/graphs.contributors>
#
# SPDX-License-Identifier: MIT

# credo:disable-for-this-file Credo.Check.Warning.BoolOperationOnSameValues
defmodule Crux.Expression.RewriteRule.IdentityLaw do
  @moduledoc """
  Rewrite rule that applies boolean identity laws to simplify expressions.

  See: https://en.wikipedia.org/wiki/Boolean_algebra#Monotone_laws

  Applies the transformations:
  - `A AND true = A`, `true AND A = A`
  - `A OR false = A`, `false OR A = A`
  """

  use Crux.Expression.RewriteRule

  import Crux.Expression, only: [b: 1]

  @impl Crux.Expression.RewriteRule
  def walk(b(expr and true)), do: expr
  def walk(b(true and expr)), do: expr
  def walk(b(expr or false)), do: expr
  def walk(b(false or expr)), do: expr
  def walk(other), do: other
end
