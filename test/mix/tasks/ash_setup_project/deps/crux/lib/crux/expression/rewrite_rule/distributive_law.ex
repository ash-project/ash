# SPDX-FileCopyrightText: 2025 crux contributors <https://github.com/ash-project/crux/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Crux.Expression.RewriteRule.DistributiveLaw do
  @moduledoc """
  Rewrite rule that applies the distributive law to convert expressions to CNF.

  See: https://en.wikipedia.org/wiki/Distributive_property

  Applies the transformations:
  - `A OR (B AND C) = (A OR B) AND (A OR C)`
  - `(A AND B) OR C = (A OR C) AND (B OR C)`

  This transformation pushes OR operations inside AND operations, which is
  necessary for achieving Conjunctive Normal Form (CNF).
  """

  use Crux.Expression.RewriteRule

  import Crux.Expression, only: [b: 1]

  alias Crux.Expression.RewriteRule

  @impl RewriteRule
  def needs_reapplication?, do: true

  @impl RewriteRule
  def walk(b(left or (right1 and right2))), do: b((left or right1) and (left or right2))
  def walk(b((left1 and left2) or right)), do: b((left1 or right) and (left2 or right))
  def walk(other), do: other
end
