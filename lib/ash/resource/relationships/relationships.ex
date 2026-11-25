# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Resource.Relationships do
  @moduledoc "Types Ash relationships"
  alias Ash.Resource.Relationships.{BelongsTo, HasMany, HasOne, ManyToMany}

  @typedoc """
  A relationship definition returned by `Ash.Resource.Info.relationship/2`.
  The struct describes its source, destination, and loading configuration.
  """
  @type relationship :: HasOne.t() | BelongsTo.t() | HasMany.t() | ManyToMany.t()
  @typedoc """
  The kind of relationship declared in the resource DSL.

  See [Relationships](/documentation/topics/resources/relationships.md) for
  definitions and examples of each kind.
  """
  @type type :: :has_many | :has_one | :belongs_to | :many_to_many
  @typedoc """
  Whether a relationship can return multiple records or at most one record.
  """
  @type cardinality :: :many | :one
end
