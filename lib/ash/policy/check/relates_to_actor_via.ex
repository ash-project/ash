defmodule Ash.Policy.Check.RelatesToActorVia do
  @moduledoc false
  use Ash.Policy.FilterCheck

  require Ash.Expr
  import Ash.Filter.TemplateHelpers

  @impl true
  def describe(opts) do
    path = Enum.join(opts[:relationship_path], ".")
    "record.#{path} == actor"
  end

  @impl true
  def filter(opts) do
    opts = Keyword.update!(opts, :relationship_path, &List.wrap/1)
    {last_relationship, to_many?} = relationship_info(opts[:resource], opts[:relationship_path])

    pkey =
      last_relationship.destination
      |> Ash.Resource.Info.primary_key()

    if to_many? do
      Ash.Expr.expr(exists(^opts[:relationship_path], ^Enum.map(pkey, &{&1, {:_actor, &1}})))
    else
      Enum.reduce(pkey, nil, fn field, expr ->
        if expr do
          Ash.Expr.expr(^expr and ^ref(opts[:relationship_path], field) == ^actor(field))
        else
          Ash.Expr.expr(^ref(opts[:relationship_path], field) == ^actor(field))
        end
      end)
    end
  end

  @impl true
  def reject(opts) do
    opts = Keyword.update!(opts, :relationship_path, &List.wrap/1)
    {last_relationship, to_many?} = relationship_info(opts[:resource], opts[:relationship_path])

    pkey =
      last_relationship.destination
      |> Ash.Resource.Info.primary_key()

    if to_many? do
      Ash.Expr.expr(not (^filter(opts)))
    else
      expr =
        Enum.reduce(pkey, nil, fn field, expr ->
          if expr do
            Ash.Expr.expr(^expr and is_nil(^ref(opts[:relationship_path], field)))
          else
            Ash.Expr.expr(is_nil(^ref(opts[:relationship_path], field)))
          end
        end)

      Ash.Expr.expr(not (^filter(opts)) or ^expr)
    end
  end

  defp relationship_info(resource, path, to_many? \\ false)

  defp relationship_info(resource, [rel], to_many?) do
    rel = Ash.Resource.Info.relationship(resource, rel)
    {rel, to_many? || rel.cardinality == :many}
  end

  defp relationship_info(resource, [rel | rest], to_many?) do
    rel = Ash.Resource.Info.relationship(resource, rel)
    relationship_info(rel.destination, rest, to_many? || rel.cardinality == :many)
  end
end
