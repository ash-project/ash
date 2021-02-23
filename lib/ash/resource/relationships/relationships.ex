defmodule Ash.Resource.Relationships do
  @moduledoc "Types Ash relationships"
  alias Ash.Resource.Relationships.{BelongsTo, HasMany, HasOne, ManyToMany}

  @type relationship :: HasOne.t() | BelongsTo.t() | HasMany.t() | ManyToMany.t()
  @type cardinality :: :many | :one
end
