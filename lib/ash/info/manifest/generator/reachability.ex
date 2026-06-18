# SPDX-FileCopyrightText: 2025 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Info.Manifest.Generator.Reachability do
  @moduledoc """
  Discovers all reachable resources and standalone types by traversing the type graph
  starting from a set of root resources.

  Returns results in depth-first discovery order: dependencies appear before the
  resources that reference them. This ordering is important for consumers that need
  to declare types before they are referenced (e.g., Zod schema generation).

  Handles cycle detection via a visited set to prevent infinite recursion.
  """

  alias Ash.Info.Manifest.Generator.TypeResolver

  @doc """
  Find all resources and standalone types reachable from the given resource modules.

  Accepts either:
  - A list of resource modules (traverses all fields, relationships, and public action arguments)
  - A list of `{resource_module, [action_name]}` tuples (only traverses arguments of specified actions)

  ## Visibility Options

    * `:include_private_attributes?` - Traverse private attributes (default: `false`)
    * `:include_private_calculations?` - Traverse private calculations (default: `false`)
    * `:include_private_aggregates?` - Traverse private aggregates (default: `false`)
    * `:include_private_relationships?` - Traverse private relationships (default: `false`)
    * `:include_private_arguments?` - Traverse private action arguments (default: `false`)
    * `:include_private_actions?` - Traverse private actions (default: `false`).
      Private actions are skipped during reachability even when their names are
      explicitly listed, so types referenced only from them don't end up in the
      manifest.

  Returns `{reachable_resources, standalone_types}` where both are lists of modules
  in depth-first discovery order (dependencies before dependents).
  """
  @spec find_reachable([atom() | {atom(), [atom()]}], keyword()) :: {[atom()], [atom()]}
  def find_reachable(resource_entries, opts \\ []) do
    {resources, types, _visited} =
      Enum.reduce(resource_entries, {[], [], MapSet.new()}, fn entry,
                                                               {resources, types, visited} ->
        {resource, action_names} = normalize_entry(entry)

        if MapSet.member?(visited, resource) do
          {resources, types, visited}
        else
          {found_resources, found_types, new_visited} =
            traverse_resource(
              resource,
              MapSet.put(visited, resource),
              action_names,
              opts
            )

          {
            resources ++ found_resources ++ [resource],
            types ++ found_types,
            new_visited
          }
        end
      end)

    {Enum.uniq(resources), Enum.uniq(types)}
  end

  defp normalize_entry({resource, action_names}) when is_atom(resource) and is_list(action_names),
    do: {resource, action_names}

  defp normalize_entry(resource) when is_atom(resource),
    do: {resource, nil}

  # action_names: nil means "traverse all public actions", list means "only these actions"
  defp traverse_resource(resource, visited, action_names, opts) do
    if is_resource?(resource) do
      # Traverse fields (respecting visibility options)
      fields = get_fields(resource, opts)

      # Traverse relationships (respecting visibility options)
      relationships = get_relationships(resource, opts)

      # Walk fields
      {field_resources, field_types, visited} =
        Enum.reduce(fields, {[], [], visited}, fn field, {resources, types, visited} ->
          {type, constraints} = get_field_type_and_constraints(field)

          {found_r, found_t, new_visited} =
            traverse_type(type, constraints, visited, opts)

          {resources ++ found_r, types ++ found_t, new_visited}
        end)

      # Walk relationship destinations
      {rel_resources, rel_types, visited} =
        Enum.reduce(
          relationships,
          {field_resources, field_types, visited},
          fn rel, {resources, types, visited} ->
            destination = rel.destination

            if MapSet.member?(visited, destination) do
              {resources, types, visited}
            else
              new_visited = MapSet.put(visited, destination)
              # Discovered resources only traverse fields/relationships, not action arguments
              {found_r, found_t, newer_visited} =
                traverse_resource(destination, new_visited, [], opts)

              {
                resources ++ found_r ++ [destination],
                types ++ found_t,
                newer_visited
              }
            end
          end
        )

      # Walk action arguments
      {arg_resources, arg_types, visited} =
        traverse_action_arguments(
          resource,
          action_names,
          rel_resources,
          rel_types,
          visited,
          opts
        )

      {arg_resources, arg_types, visited}
    else
      {[], [], visited}
    end
  end

  defp traverse_action_arguments(
         resource,
         action_names,
         resources,
         types,
         visited,
         opts
       ) do
    actions =
      resource
      |> get_actions_to_traverse(action_names)
      |> filter_private_actions(opts)

    include_private_args? = Keyword.get(opts, :include_private_arguments?, false)

    Enum.reduce(actions, {resources, types, visited}, fn action, {resources, types, visited} ->
      args =
        if include_private_args? do
          action.arguments
        else
          Enum.filter(action.arguments, & &1.public?)
        end

      # Walk argument types
      {resources, types, visited} =
        Enum.reduce(args, {resources, types, visited}, fn arg, {resources, types, visited} ->
          {type, constraints} = {arg.type, arg.constraints || []}

          {found_r, found_t, new_visited} =
            traverse_type(type, constraints, visited, opts)

          {resources ++ found_r, types ++ found_t, new_visited}
        end)

      # Walk accepted-attribute types. Public attributes are already reached
      # via the resource's field set, but private attributes named in `accept`
      # become action inputs and their types must be registered too.
      {resources, types, visited} =
        traverse_action_accepted_attributes(resource, action, resources, types, visited, opts)

      # Walk return type (generic actions declare a custom return type via :returns;
      # CRUD actions return the resource itself, which is already reachable)
      {resources, types, visited} =
        traverse_action_returns(action, resources, types, visited, opts)

      # Walk metadata field types (custom types in metadata fields need to be reachable)
      traverse_action_metadata(action, resources, types, visited, opts)
    end)
  end

  defp traverse_action_accepted_attributes(resource, action, resources, types, visited, opts) do
    accept = Map.get(action, :accept) || []

    Enum.reduce(accept, {resources, types, visited}, fn name, {resources, types, visited} ->
      case Ash.Resource.Info.attribute(resource, name) do
        nil ->
          {resources, types, visited}

        attribute ->
          {found_r, found_t, new_visited} =
            traverse_type(attribute.type, attribute.constraints || [], visited, opts)

          {resources ++ found_r, types ++ found_t, new_visited}
      end
    end)
  end

  defp traverse_action_returns(action, resources, types, visited, opts) do
    case Map.get(action, :returns) do
      nil ->
        {resources, types, visited}

      return_type ->
        return_constraints = Map.get(action, :constraints) || []

        {found_r, found_t, new_visited} =
          traverse_type(return_type, return_constraints, visited, opts)

        {resources ++ found_r, types ++ found_t, new_visited}
    end
  end

  defp traverse_action_metadata(action, resources, types, visited, opts) do
    metadata = Map.get(action, :metadata) || []

    Enum.reduce(metadata, {resources, types, visited}, fn meta, {resources, types, visited} ->
      meta_type = Map.get(meta, :type)
      meta_constraints = Map.get(meta, :constraints) || []

      if meta_type do
        {found_r, found_t, new_visited} =
          traverse_type(meta_type, meta_constraints, visited, opts)

        {resources ++ found_r, types ++ found_t, new_visited}
      else
        {resources, types, visited}
      end
    end)
  end

  defp get_actions_to_traverse(resource, nil) do
    # nil means traverse all public actions
    Ash.Resource.Info.actions(resource)
    |> Enum.filter(&Map.get(&1, :public?, false))
  end

  defp get_actions_to_traverse(resource, action_names) when is_list(action_names) do
    Enum.flat_map(action_names, fn name ->
      case Ash.Resource.Info.action(resource, name) do
        nil -> []
        action -> [action]
      end
    end)
  end

  defp filter_private_actions(actions, opts) do
    if Keyword.get(opts, :include_private_actions?, false) do
      actions
    else
      Enum.filter(actions, &Map.get(&1, :public?, true))
    end
  end

  defp traverse_type(type, constraints, visited, opts) when is_list(constraints) do
    {unwrapped_type, unwrapped_constraints} = TypeResolver.unwrap_new_type(type, constraints)

    # Detect named type modules: NewTypes (type differs after unwrap) or enums
    is_named = is_atom(type) and TypeResolver.named_type_module?(type)

    if is_named and MapSet.member?(visited, type) do
      # Cycle detected — stop traversal
      {[], [], visited}
    else
      visited = if is_named, do: MapSet.put(visited, type), else: visited

      {found_r, found_t, visited} =
        traverse_unwrapped_type(unwrapped_type, unwrapped_constraints, visited, opts)

      if is_named do
        {found_r, [type | found_t], visited}
      else
        {found_r, found_t, visited}
      end
    end
  end

  defp traverse_type(_type, _constraints, visited, _opts) do
    {[], [], visited}
  end

  defp traverse_unwrapped_type(unwrapped_type, constraints, visited, opts) do
    case unwrapped_type do
      {:array, inner_type} ->
        items_constraints = Keyword.get(constraints, :items, [])
        traverse_type(inner_type, items_constraints, visited, opts)

      Ash.Type.Struct ->
        instance_of = Keyword.get(constraints, :instance_of)

        if instance_of && is_resource?(instance_of) do
          traverse_resource_ref(instance_of, visited, opts)
        else
          traverse_field_constraints(constraints, visited, opts)
        end

      Ash.Type.Union ->
        union_types = Keyword.get(constraints, :types, [])

        Enum.reduce(union_types, {[], [], visited}, fn {_name, config},
                                                       {resources, types, visited} ->
          member_type = Keyword.get(config, :type)
          member_constraints = Keyword.get(config, :constraints, [])

          if member_type do
            {found_r, found_t, new_visited} =
              traverse_type(member_type, member_constraints, visited, opts)

            {resources ++ found_r, types ++ found_t, new_visited}
          else
            {resources, types, visited}
          end
        end)

      type when type in [Ash.Type.Map, Ash.Type.Keyword, Ash.Type.Tuple] ->
        traverse_field_constraints(constraints, visited, opts)

      type when is_atom(type) ->
        cond do
          is_resource?(type) ->
            traverse_resource_ref(type, visited, opts)

          Code.ensure_loaded?(type) == true ->
            traverse_field_constraints(constraints, visited, opts)

          true ->
            {[], [], visited}
        end

      _ ->
        {[], [], visited}
    end
  end

  defp traverse_resource_ref(resource, visited, opts) do
    if MapSet.member?(visited, resource) do
      {[], [], visited}
    else
      new_visited = MapSet.put(visited, resource)
      # Discovered resources only traverse fields/relationships, not action arguments
      {found_r, found_t, newer_visited} =
        traverse_resource(resource, new_visited, [], opts)

      {found_r ++ [resource], found_t, newer_visited}
    end
  end

  defp traverse_field_constraints(constraints, visited, opts) do
    fields = Keyword.get(constraints, :fields)

    if fields && is_list(fields) do
      Enum.reduce(fields, {[], [], visited}, fn {_name, config}, {resources, types, visited} ->
        field_type = Keyword.get(config, :type)
        field_constraints = Keyword.get(config, :constraints, [])

        if field_type do
          {found_r, found_t, new_visited} =
            traverse_type(field_type, field_constraints, visited, opts)

          {resources ++ found_r, types ++ found_t, new_visited}
        else
          {resources, types, visited}
        end
      end)
    else
      {[], [], visited}
    end
  end

  defp get_fields(resource, opts) do
    include_private_attrs? = Keyword.get(opts, :include_private_attributes?, false)
    include_private_calcs? = Keyword.get(opts, :include_private_calculations?, false)
    include_private_aggs? = Keyword.get(opts, :include_private_aggregates?, false)

    # If all three are the same (all public or all include-private), fetch in one call
    if include_private_attrs? == include_private_calcs? and
         include_private_calcs? == include_private_aggs? do
      if include_private_attrs? do
        Ash.Resource.Info.fields(resource, [:attributes, :aggregates, :calculations])
      else
        resource
        |> Ash.Resource.Info.fields([:attributes, :aggregates, :calculations])
        |> Enum.filter(& &1.public?)
      end
    else
      # Mix-and-match: fetch each kind separately
      attrs =
        if include_private_attrs?,
          do: Ash.Resource.Info.attributes(resource),
          else: Ash.Resource.Info.public_attributes(resource)

      calcs =
        if include_private_calcs?,
          do: Ash.Resource.Info.calculations(resource),
          else: Ash.Resource.Info.public_calculations(resource)

      aggs =
        if include_private_aggs?,
          do: Ash.Resource.Info.aggregates(resource),
          else: Ash.Resource.Info.public_aggregates(resource)

      Enum.concat([attrs, calcs, aggs])
    end
  end

  defp get_relationships(resource, opts) do
    if Keyword.get(opts, :include_private_relationships?, false) do
      Ash.Resource.Info.relationships(resource)
    else
      Ash.Resource.Info.public_relationships(resource)
    end
  end

  defp get_field_type_and_constraints(field) do
    {Map.get(field, :type), Map.get(field, :constraints, []) || []}
  end

  defp is_resource?(module) when is_atom(module) do
    Code.ensure_loaded?(module) == true and Ash.Resource.Info.resource?(module)
  end

  defp is_resource?(_), do: false
end
