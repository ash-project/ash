# SPDX-FileCopyrightText: 2020 Zach Daniel
#
# SPDX-License-Identifier: MIT

# credo:disable-for-this-file Credo.Check.Warning.BoolOperationOnSameValues
defmodule Ash.SatSolver.Expression.RewriteRule.IdempotentLaw do
  @moduledoc false

  # Rewrite rule that applies idempotent laws to simplify expressions.
  #
  # See: https://en.wikipedia.org/wiki/Idempotence
  #
  # Applies the transformations:
  # - `A AND A = A`
  # - `A OR A = A`
  #
  # The idempotent laws state that applying the same operation twice
  # has the same effect as applying it once.

  use Ash.SatSolver.Expression.RewriteRule

  import Ash.SatSolver.Expression, only: [b: 1]

  @impl Ash.SatSolver.Expression.RewriteRule
  def walk(b(expr and expr)), do: expr
  def walk(b(expr or expr)), do: expr
  def walk(other), do: other
end
