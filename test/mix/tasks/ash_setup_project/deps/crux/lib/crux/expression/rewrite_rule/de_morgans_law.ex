# SPDX-FileCopyrightText: 2025 crux contributors <https://github.com/ash-project/crux/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Crux.Expression.RewriteRule.DeMorgansLaw do
  @moduledoc """
  Rewrite rule that applies De Morgan's laws to expressions.

  See: https://en.wikipedia.org/wiki/De_Morgan%27s_laws

  Applies the transformations:
  - `NOT (A AND B) = (NOT A) OR (NOT B)`
  - `NOT (A OR B) = (NOT A) AND (NOT B)`
  """

  use Crux.Expression.RewriteRule

  import Crux.Expression, only: [b: 1]

  alias Crux.Expression.RewriteRule

  @impl RewriteRule
  def exclusive?, do: true

  @impl RewriteRule
  def needs_reapplication?, do: true

  @impl RewriteRule
  def walk(b(nand(left, right))), do: b(not left or not right)
  def walk(b(nor(left, right))), do: b(not left and not right)
  def walk(other), do: other
end
