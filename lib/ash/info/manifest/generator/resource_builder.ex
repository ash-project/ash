# SPDX-FileCopyrightText: 2025 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Info.Manifest.Generator.ResourceBuilder do
  @moduledoc """
  Converts an Ash resource module into an `%Ash.Info.Manifest.Resource{}` struct.
  """

  alias Ash.Info.Manifest.{Argument, Field, Relationship, Resource}

  alias Ash.Info.Manifest.Generator.{
    OperatorResolver,
    ResultNullability,
    TypeResolver
  }

  @doc """
  Build a `%Ash.Info.Manifest.Resource{}` from an Ash resource module.

  Resources are pure type/shape definitions — actions live in entrypoints.

  ## Options

    * `:include_private_attributes?` - Include private attributes (default: `false`)
    * `:include_private_calculations?` - Include private calculations (default: `false`)
    * `:include_private_aggregates?` - Include private aggregates (default: `false`)
    * `:include_private_relationships?` - Include private relationships (default: `false`)
  """
  @spec build(atom(), keyword(), Ash.Info.Manifest.FilterCapabilities.t() | nil) :: Resource.t()
  def build(resource, opts \\ [], filter_capabilities \\ nil) do
    data_layer = Ash.DataLayer.data_layer(resource)

    %Resource{
      name: resource_name(resource),
      module: resource,
      embedded?: Ash.Resource.Info.embedded?(resource),
      primary_key: Ash.Resource.Info.primary_key(resource),
      description: Ash.Resource.Info.description(resource),
      fields: build_fields(resource, opts, filter_capabilities, data_layer),
      relationships: build_relationships(resource, opts),
      identities: build_identities(resource),
      multitenancy: build_multitenancy(resource)
    }
  end

  defp build_fields(resource, opts, filter_capabilities, data_layer) do
    attributes = build_attributes(resource, opts, filter_capabilities, data_layer)
    calculations = build_calculations(resource, opts, filter_capabilities, data_layer)
    aggregates = build_aggregates(resource, opts, filter_capabilities, data_layer)

    Map.new(attributes ++ calculations ++ aggregates, fn field -> {field.name, field} end)
  end

  defp build_attributes(resource, opts, filter_capabilities, data_layer) do
    resource
    |> get_attributes(opts)
    |> Enum.map(fn attr ->
      type = TypeResolver.resolve(attr.type, attr.constraints || [])
      filterable? = Map.get(attr, :filterable?, true)

      %Field{
        name: attr.name,
        kind: :attribute,
        type: type,
        allow_nil?: attr.allow_nil?,
        writable?: attr.writable?,
        has_default?: not is_nil(attr.default),
        description: Map.get(attr, :description),
        filterable?: filterable?,
        sortable?: Map.get(attr, :sortable?, true),
        primary_key?: attr.primary_key?,
        sensitive?: Map.get(attr, :sensitive?, false),
        select_by_default?: Map.get(attr, :select_by_default?, true)
      }
      |> resolve_filter_lists(filterable?, type, filter_capabilities, data_layer)
    end)
  end

  defp build_calculations(resource, opts, filter_capabilities, data_layer) do
    resource
    |> get_calculations(opts)
    |> Enum.filter(fn calc -> Map.get(calc, :field?, true) end)
    |> Enum.map(fn calc ->
      arguments =
        Enum.map(calc.arguments, fn arg ->
          %Argument{
            name: arg.name,
            type: TypeResolver.resolve(arg.type, arg.constraints || []),
            allow_nil?: arg.allow_nil?,
            has_default?: not is_nil(arg.default),
            description: Map.get(arg, :description),
            sensitive?: Map.get(arg, :sensitive?, false)
          }
        end)

      type = TypeResolver.resolve(calc.type, calc.constraints || [])
      filterable? = Map.get(calc, :filterable?, true)

      %Field{
        name: calc.name,
        kind: :calculation,
        type: type,
        allow_nil?: calc.allow_nil?,
        writable?: false,
        has_default?: false,
        description: Map.get(calc, :description),
        filterable?: filterable?,
        sortable?: Map.get(calc, :sortable?, true),
        primary_key?: false,
        sensitive?: Map.get(calc, :sensitive?, false),
        select_by_default?: Map.get(calc, :select_by_default?, true),
        arguments: arguments
      }
      |> resolve_filter_lists(filterable?, type, filter_capabilities, data_layer)
    end)
  end

  defp build_aggregates(resource, opts, filter_capabilities, data_layer) do
    resource
    |> get_aggregates(opts)
    |> Enum.map(fn aggregate ->
      {type_in, constraints} = resolve_aggregate_type(resource, aggregate)
      type = TypeResolver.resolve(type_in, constraints)
      filterable? = Map.get(aggregate, :filterable?, true)

      %Field{
        name: aggregate.name,
        kind: :aggregate,
        type: type,
        allow_nil?: ResultNullability.for_aggregate(aggregate),
        writable?: false,
        has_default?: false,
        description: Map.get(aggregate, :description),
        filterable?: filterable?,
        sortable?: Map.get(aggregate, :sortable?, true),
        primary_key?: false,
        sensitive?: false,
        select_by_default?: Map.get(aggregate, :select_by_default?, true),
        aggregate_kind: aggregate.kind
      }
      |> resolve_filter_lists(filterable?, type, filter_capabilities, data_layer)
    end)
  end

  defp resolve_filter_lists(field, false, _type, _caps, _data_layer) do
    %{
      field
      | filter_operators: nil,
        filter_functions: nil,
        filter_custom_expressions: nil
    }
  end

  defp resolve_filter_lists(field, true, _type, nil, _data_layer) do
    %{
      field
      | filter_operators: nil,
        filter_functions: nil,
        filter_custom_expressions: nil
    }
  end

  defp resolve_filter_lists(field, true, type, capabilities, data_layer) do
    {operators, functions} = OperatorResolver.resolve(type, capabilities, data_layer)
    custom_expressions = OperatorResolver.resolve_custom_expressions(type, capabilities)

    %{
      field
      | filter_operators: operators,
        filter_functions: functions,
        filter_custom_expressions: custom_expressions
    }
  end

  defp resolve_aggregate_type(resource, aggregate) do
    field =
      if aggregate.field do
        related = Ash.Resource.Info.related(resource, aggregate.relationship_path)

        if related do
          Ash.Resource.Info.attribute(related, aggregate.field) ||
            Ash.Resource.Info.calculation(related, aggregate.field)
        end
      end

    field_type = if field, do: field.type
    field_constraints = if field, do: Map.get(field, :constraints, []), else: []

    case Ash.Query.Aggregate.kind_to_type(aggregate.kind, field_type, field_constraints) do
      {:ok, type, constraints} -> {type, constraints}
      _other -> {aggregate.type, aggregate.constraints || []}
    end
  end

  defp build_relationships(resource, opts) do
    resource
    |> get_relationships(opts)
    |> Map.new(fn rel ->
      relationship = %Relationship{
        name: rel.name,
        type: relationship_type(rel),
        cardinality: relationship_cardinality(rel),
        destination: rel.destination,
        allow_nil?: Map.get(rel, :allow_nil?, true),
        description: Map.get(rel, :description),
        filterable?: Map.get(rel, :filterable?, true),
        sortable?: Map.get(rel, :sortable?, true)
      }

      {rel.name, relationship}
    end)
  end

  defp relationship_type(%Ash.Resource.Relationships.HasMany{}), do: :has_many
  defp relationship_type(%Ash.Resource.Relationships.HasOne{}), do: :has_one
  defp relationship_type(%Ash.Resource.Relationships.BelongsTo{}), do: :belongs_to
  defp relationship_type(%Ash.Resource.Relationships.ManyToMany{}), do: :many_to_many

  defp relationship_cardinality(%Ash.Resource.Relationships.HasMany{}), do: :many
  defp relationship_cardinality(%Ash.Resource.Relationships.ManyToMany{}), do: :many
  defp relationship_cardinality(_), do: :one

  defp build_identities(resource) do
    resource
    |> Ash.Resource.Info.identities()
    |> Map.new(fn identity ->
      {identity.name, %{keys: identity.keys}}
    end)
  end

  defp build_multitenancy(resource) do
    strategy = Ash.Resource.Info.multitenancy_strategy(resource)

    if strategy do
      %{
        strategy: strategy,
        global?: Ash.Resource.Info.multitenancy_global?(resource),
        attribute: Ash.Resource.Info.multitenancy_attribute(resource)
      }
    else
      nil
    end
  end

  defp get_attributes(resource, opts) do
    if Keyword.get(opts, :include_private_attributes?, false) do
      Ash.Resource.Info.attributes(resource)
    else
      Ash.Resource.Info.public_attributes(resource)
    end
  end

  defp get_calculations(resource, opts) do
    if Keyword.get(opts, :include_private_calculations?, false) do
      Ash.Resource.Info.calculations(resource)
    else
      Ash.Resource.Info.public_calculations(resource)
    end
  end

  defp get_aggregates(resource, opts) do
    if Keyword.get(opts, :include_private_aggregates?, false) do
      Ash.Resource.Info.aggregates(resource)
    else
      Ash.Resource.Info.public_aggregates(resource)
    end
  end

  defp get_relationships(resource, opts) do
    if Keyword.get(opts, :include_private_relationships?, false) do
      Ash.Resource.Info.relationships(resource)
    else
      Ash.Resource.Info.public_relationships(resource)
    end
  end

  defp resource_name(module) do
    module
    |> Module.split()
    |> List.last()
  end
end
