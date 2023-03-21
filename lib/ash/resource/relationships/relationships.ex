defmodule Ash.Resource.Relationships do
  @moduledoc "Types Ash relationships"
  alias Ash.Resource.Relationships.{BelongsTo, HasMany, HasOne, ManyToMany}

  @type relationship :: HasOne.t() | BelongsTo.t() | HasMany.t() | ManyToMany.t()
  @type type :: :has_many | :has_one | :belongs_to | :many_to_many
  @type cardinality :: :many | :one
end
