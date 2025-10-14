# SPDX-FileCopyrightText: ash contributors <https://github.com/ash-project/ash/graphs.contributors>
#
#
# SPDX-License-Identifier: MIT

defmodule Ash.SatSolver.Expression.RewriteRule.DeMorgansLaw do
  @moduledoc false

  # Rewrite rule that applies De Morgan's laws to expressions.
  #
  # See: https://en.wikipedia.org/wiki/De_Morgan%27s_laws
  #
  # Applies the transformations:
  # - `NOT (A AND B) = (NOT A) OR (NOT B)`
  # - `NOT (A OR B) = (NOT A) AND (NOT B)`

  use Ash.SatSolver.Expression.RewriteRule

  import Ash.SatSolver.Expression, only: [b: 1]

  @impl Ash.SatSolver.Expression.RewriteRule
  def exclusive?, do: true

  @impl Ash.SatSolver.Expression.RewriteRule
  def needs_reapplication?, do: true

  @impl Ash.SatSolver.Expression.RewriteRule
  def walk(b(nand(left, right))), do: b(not left or not right)
  def walk(b(nor(left, right))), do: b(not left and not right)
  def walk(other), do: other
end
