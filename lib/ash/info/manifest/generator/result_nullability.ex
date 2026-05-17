# SPDX-FileCopyrightText: 2025 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Info.Manifest.Generator.ResultNullability do
  @moduledoc """
  Computes manifest-level `allow_nil?` (result nullability) for aggregates.

  Per Ash's aggregate semantics, this is independent of the user-facing
  `include_nil?` flag (which controls whether nils contribute to the aggregated
  set — an *input* concern). The result nullability is determined by the
  aggregate's `kind`:

    * `:count`, `:exists`, `:list` — never null. `:count` returns 0 on an empty
      relationship, `:exists` returns false, `:list` returns `[]`.
    * `:first`, `:max`, `:min`, `:sum`, `:avg` — null on an empty relationship.
    * `:custom` — preserves the aggregate's own `allow_nil?` if set; otherwise
      defaults to `true` (the safe assumption, since the callback's behavior is
      unknown).
  """

  @spec for_aggregate(map()) :: boolean()
  def for_aggregate(%{kind: kind} = aggregate) do
    case kind do
      k when k in [:count, :exists, :list] -> false
      k when k in [:first, :max, :min, :sum, :avg] -> true
      :custom -> Map.get(aggregate, :allow_nil?, true)
    end
  end
end
