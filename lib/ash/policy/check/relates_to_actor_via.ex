defmodule Ash.Policy.Check.RelatesToActorVia do
  @moduledoc false
  use Ash.Policy.FilterCheck

  require Ash.Expr

  @impl true
  def describe(opts) do
    path = Enum.join(opts[:relationship_path], ".")
    "record.#{path} == actor"
  end

  @impl true
  def filter(opts) do
    pkey =
      opts[:resource]
      |> Ash.Resource.Info.related(opts[:relationship_path])
      |> Kernel.||(raise "Must be able to determine related resource for `relates_to_actor_via`")
      |> Ash.Resource.Info.primary_key()

    Ash.Expr.expr(exists(^opts[:relationship_path], ^Enum.map(pkey, &{&1, {:_actor, &1}})))
  end
end

# defmodule Ash.Policy.Check.RelatesToActorVia do
#   @moduledoc false
#   use Ash.Policy.FilterCheck

#   require Ash.Expr

#   @impl true
#   def describe(opts) do
#     path = Enum.join(opts[:relationship_path], ".")
#     "record.#{path} == actor"
#   end

#   @impl true
#   def filter(opts) do
#     {last_relationship, to_many?} = relationship_info(opts[:resource], opts[:relationship_path])

#     pkey =
#       last_relationship.destination
#       |> Ash.Resource.Info.primary_key()

#     if to_many? do
#       Ash.Expr.expr(exists(^opts[:relationship_path], ^Enum.map(pkey, &{&1, {:_actor, &1}})))
#     else
#       put_in_path(opts[:relationship_path], Enum.map(pkey, &{&1, {:_actor, &1}}))
#     end
#   end

#   @impl true
#   def reject(opts) do
#     {last_relationship, to_many?} = relationship_info(opts[:resource], opts[:relationship_path])

#     pkey =
#       last_relationship.destination
#       |> Ash.Resource.Info.primary_key()

#     if to_many? do
#       Ash.Expr.expr(not exists(^opts[:relationship_path], ^Enum.map(pkey, &{&1, {:_actor, &1}})))
#     else
#       [
#         or: [
#           [not: filter(opts)],
#           [put_in_path(opts[:relationship_path], Enum.map(pkey, &{:is_nil, &1}))]
#         ]
#       ]
#     end
#   end

#   defp relationship_info(resource, path, to_many? \\ false)

#   defp relationship_info(resource, [rel], to_many?) do
#     rel = Ash.Resource.Info.relationship(resource, rel)
#     {rel, to_many? || rel.cardinality == :many}
#   end

#   defp relationship_info(resource, [rel | rest], to_many?) do
#     rel = Ash.Resource.Info.relationship(resource, rel)
#     relationship_info(rel.destination, rest, to_many? || rel.cardinality == :many)
#   end

#   defp put_in_path([], value) do
#     value
#   end

#   defp put_in_path([key | rest], value) do
#     [{key, put_in_path(rest, value)}]
#   end
# end
