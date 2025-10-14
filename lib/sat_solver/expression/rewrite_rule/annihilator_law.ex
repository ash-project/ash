# SPDX-FileCopyrightText: 2020 Zach Daniel
#
# SPDX-License-Identifier: MIT

defmodule Ash.SatSolver.Expression.RewriteRule.AnnihilatorLaw do
  @moduledoc false

  # Rewrite rule that applies boolean annihilator laws.
  #
  # See: https://en.wikipedia.org/wiki/Boolean_algebra#Monotone_laws
  #
  # Applies the transformations:
  # - `A AND false = false` (false annihilates AND)
  # - `false AND A = false` (false annihilates AND)
  # - `A OR true = true` (true annihilates OR)
  # - `true OR A = true` (true annihilates OR)
  #
  # The annihilator laws state that certain values (false for AND, true for OR)
  # completely dominate the result regardless of other operands.

  use Ash.SatSolver.Expression.RewriteRule

  import Ash.SatSolver.Expression, only: [b: 1]

  @impl Ash.SatSolver.Expression.RewriteRule
  def walk(b(_expr and false)), do: false
  def walk(b(false and _expr)), do: false
  def walk(b(_expr or true)), do: true
  def walk(b(true or _expr)), do: true
  def walk(other), do: other
end
