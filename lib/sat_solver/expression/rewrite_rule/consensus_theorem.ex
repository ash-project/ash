# SPDX-FileCopyrightText: 2020 Zach Daniel
#
# SPDX-License-Identifier: MIT

defmodule Ash.SatSolver.Expression.RewriteRule.ConsensusTheorem do
  @moduledoc false

  # Rewrite rule that applies the consensus theorem to eliminate redundant clauses.
  #
  # See: https://en.wikipedia.org/wiki/Consensus_theorem
  #
  # Applies the transformations:
  # - `(A OR B) AND (NOT A OR C) AND (B OR C) = (A OR B) AND (NOT A OR C)`
  # - `(A AND B) OR (NOT A AND C) OR (B AND C) = (A AND B) OR (NOT A AND C)`
  #
  # The consensus theorem states that in certain patterns, one clause can be
  # derived from two others and is therefore redundant.

  use Ash.SatSolver.Expression.RewriteRule

  import Ash.SatSolver.Expression, only: [b: 1]

  @impl Ash.SatSolver.Expression.RewriteRule
  def walk(b((a or b) and (not a or c) and (b or c))), do: b((a or b) and (not a or c))
  def walk(b((not a or c) and (a or b) and (b or c))), do: b((not a or c) and (a or b))
  def walk(b((b or c) and (a or b) and (not a or c))), do: b((a or b) and (not a or c))
  def walk(b((a and b) or (not a and c) or (b and c))), do: b((a and b) or (not a and c))
  def walk(b((not a and c) or (a and b) or (b and c))), do: b((not a and c) or (a and b))
  def walk(b((b and c) or (a and b) or (not a and c))), do: b((a and b) or (not a and c))
  def walk(other), do: other
end
