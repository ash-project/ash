# SPDX-FileCopyrightText: 2025 crux contributors <https://github.com/ash-project/crux/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Crux.Expression.RewriteRule.UnitResolution do
  @moduledoc """
  Rewrite rule that applies unit resolution to propagate unit clauses.

  See: https://en.wikipedia.org/wiki/Unit_propagation

  Applies the transformations:
  - `A AND (NOT A OR B) = A AND B` (unit A eliminates NOT A from clause)
  - `(NOT A OR B) AND A = B AND A` (unit A eliminates NOT A from clause)
  - `(NOT A) AND (A OR B) = (NOT A) AND B` (unit NOT A eliminates A from clause)
  - `(A OR B) AND (NOT A) = B AND (NOT A)` (unit NOT A eliminates A from clause)

  Unit resolution propagates the effect of unit clauses (single literals)
  by eliminating contradictory literals from other clauses.
  """

  use Crux.Expression.RewriteRule

  import Crux.Expression, only: [b: 1]

  alias Crux.Expression.RewriteRule

  @impl RewriteRule
  def needs_reapplication?, do: true

  @impl RewriteRule
  def walk(b(a and (not a or b))), do: b(a and b)
  def walk(b((not a or b) and a)), do: b(b and a)
  def walk(b(not a and (a or b))), do: b(not a and b)
  def walk(b((a or b) and not a)), do: b(b and not a)
  def walk(other), do: other
end
