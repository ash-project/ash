# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Policy.Check.RelatesToActorVia do
  @moduledoc "This check passes if the data relates to the actor via the specified relationship or path of relationships."
  use Ash.Policy.FilterCheck

  @impl true
  def describe(opts) do
    path = Enum.join(opts[:relationship_path], ".")
    "record.#{path} == actor"
  end

  @impl true
  def filter(actor, context, opts) do
    opts = Keyword.update!(opts, :relationship_path, &List.wrap/1)
    actor_field = Keyword.get(opts, :field)
    {last_relationship, to_many?} = relationship_info(context.resource, opts[:relationship_path])

    pkey =
      last_relationship.destination
      |> Ash.Resource.Info.primary_key()

    if to_many? || opts[:ash_field_policy?] do
      expr(
        exists(
          ^opts[:relationship_path],
          ^Enum.map(pkey, fn pkey_field ->
            {pkey_field, {:_actor, resolve_actor_path(actor, actor_field, pkey_field)}}
          end)
        )
      )
    else
      Enum.reduce(pkey, nil, fn pkey_field, expr ->
        actor_path = resolve_actor_path(actor, actor_field, pkey_field)

        if expr do
          expr(^expr and ^ref(opts[:relationship_path], pkey_field) == ^actor(actor_path))
        else
          expr(^ref(opts[:relationship_path], pkey_field) == ^actor(actor_path))
        end
      end)
    end
  end

  defp resolve_actor_path(actor, nil, pkey_field) do
    validate_actor_field_loaded!(actor, [pkey_field])
    pkey_field
  end

  defp resolve_actor_path(actor, actor_field, pkey_field) do
    cond do
      is_nil(actor) ->
        [actor_field, pkey_field]

      is_struct(actor) and Ash.Resource.Info.resource?(actor.__struct__) ->
        case Ash.Resource.Info.relationship(actor.__struct__, actor_field) do
          %{type: :belongs_to, source_attribute: source_attr, destination_attribute: dest_attr}
          when dest_attr == pkey_field ->
            # For belongs_to, prefer the source_attribute, but fall back to relationship path
            case Map.get(actor, source_attr) do
              %Ash.NotLoaded{} ->
                # Source attribute not loaded, try the relationship path
                # Hint towards loading the source_attribute as the more optimal choice
                validate_actor_field_loaded!(actor, [actor_field, pkey_field], source_attr)
                [actor_field, pkey_field]

              _ ->
                source_attr
            end

          rel when not is_nil(rel) ->
            # Other relationship types - need to traverse the relationship
            validate_actor_field_loaded!(actor, [actor_field, pkey_field])
            [actor_field, pkey_field]

          nil ->
            # No relationship found, might be a direct field access
            validate_actor_field_loaded!(actor, [actor_field, pkey_field])
            [actor_field, pkey_field]
        end

      true ->
        # Not an Ash resource, validate and use path
        validate_actor_field_loaded!(actor, [actor_field, pkey_field])
        [actor_field, pkey_field]
    end
  end

  defp validate_actor_field_loaded!(actor, path, optimal_field \\ nil) do
    case actor_get_path(actor, path) do
      %Ash.NotLoaded{} ->
        hint =
          if optimal_field do
            "\n\nHint: Loading `#{inspect(optimal_field)}` is more optimal than loading the full relationship."
          else
            ""
          end

        raise ArgumentError, """
        Actor field is not loaded: #{inspect(path)}

        Actor: #{inspect(actor)}

        Ensure the field is loaded on the actor before using it in a `relates_to_actor_via` check.#{hint}
        """

      _ ->
        :ok
    end
  end

  defp actor_get_path(nil, _), do: nil
  defp actor_get_path(%Ash.NotLoaded{} = not_loaded, _), do: not_loaded
  defp actor_get_path(map, [key]) when is_map(map), do: Map.get(map, key)

  defp actor_get_path(map, [key | rest]) when is_map(map),
    do: actor_get_path(Map.get(map, key), rest)

  defp actor_get_path(_, _), do: nil

  @impl true
  def reject(actor, context, opts) do
    opts = Keyword.update!(opts, :relationship_path, &List.wrap/1)
    {last_relationship, to_many?} = relationship_info(context.resource, opts[:relationship_path])

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
