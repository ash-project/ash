defmodule Ash.Policy.Check.RelatesToActorVia do
  @moduledoc "This check passes if the data relates to the actor via the specified relationship or path of relationships."
  use Ash.Policy.FilterCheck

  @impl true
  def describe(opts) do
    path = Enum.join(opts[:relationship_path], ".")
    "record.#{path} == actor"
  end

  @impl true
  def filter(_actor, _context, opts) do
    opts = Keyword.update!(opts, :relationship_path, &List.wrap/1)
    actor_field = Keyword.get(opts, :field)
    {last_relationship, to_many?} = relationship_info(opts[:resource], opts[:relationship_path])

    pkey =
      last_relationship.destination
      |> Ash.Resource.Info.primary_key()

    if to_many? || opts[:ash_field_policy?] do
      expr(
        exists(
          ^opts[:relationship_path],
          ^Enum.map(pkey, &{&1, {:_actor, actor_path(&1, actor_field)}})
        )
      )
    else
      Enum.reduce(pkey, nil, fn pkey_field, expr ->
        actor_path = actor_path(pkey_field, actor_field)

        if expr do
          expr(^expr and ^ref(opts[:relationship_path], pkey_field) == ^actor(actor_path))
        else
          expr(^ref(opts[:relationship_path], pkey_field) == ^actor(actor_path))
        end
      end)
    end
  end

  defp actor_path(pkey_field, actor_field) when is_nil(actor_field), do: pkey_field
  defp actor_path(pkey_field, actor_field), do: [actor_field, pkey_field]

  @impl true
  def reject(actor, context, opts) do
    opts = Keyword.update!(opts, :relationship_path, &List.wrap/1)
    {last_relationship, to_many?} = relationship_info(opts[:resource], opts[:relationship_path])

    pkey =
      last_relationship.destination
      |> Ash.Resource.Info.primary_key()

    if to_many? do
      Ash.Expr.expr(not (^filter(actor, context, opts)))
    else
      expr =
        Enum.reduce(pkey, nil, fn field, expr ->
          if expr do
            Ash.Expr.expr(^expr and is_nil(^ref(opts[:relationship_path], field)))
          else
            Ash.Expr.expr(is_nil(^ref(opts[:relationship_path], field)))
          end
        end)

      Ash.Expr.expr(not (^filter(actor, context, opts)) or ^expr)
    end
  end

  defp relationship_info(resource, path, to_many? \\ false)

  defp relationship_info(resource, [rel_key], to_many?) do
    rel = Ash.Resource.Info.relationship(resource, rel_key)

    raise_if_nil(rel, rel_key, resource)

    {rel, to_many? || rel.cardinality == :many}
  end

  defp relationship_info(resource, [rel_key | rest], to_many?) do
    rel = Ash.Resource.Info.relationship(resource, rel_key)

    raise_if_nil(rel, rel_key, resource)

    relationship_info(rel.destination, rest, to_many? || rel.cardinality == :many)
  end

  defp raise_if_nil(nil, rel_key, resource) do
    raise "No such relationship ':#{rel_key}' for #{resource}, required in `relates_to_actor` check"
  end

  defp raise_if_nil(_, _, _) do
    :ok
  end
end
