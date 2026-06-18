# SPDX-FileCopyrightText: 2025 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Info.Manifest.Resource do
  @moduledoc """
  Represents a resource in the API specification.

  Resources are pure type/shape definitions. Fields and relationships are
  stored as maps keyed by atom name for O(1) lookup. Actions live separately
  in `%Ash.Info.Manifest{}.entrypoints`.
  """

  @type t :: %__MODULE__{
          name: String.t(),
          module: atom(),
          embedded?: boolean(),
          primary_key: [atom()],
          description: String.t() | nil,
          fields: %{atom() => Ash.Info.Manifest.Field.t()},
          relationships: %{atom() => Ash.Info.Manifest.Relationship.t()},
          identities: %{atom() => %{keys: [atom()]}},
          multitenancy: %{strategy: atom(), global?: boolean(), attribute: atom()} | nil,
          custom: map()
        }

  defstruct [
    :name,
    :module,
    :embedded?,
    :primary_key,
    :description,
    :multitenancy,
    fields: %{},
    relationships: %{},
    identities: %{},
    custom: %{}
  ]

  @doc "Gets a field (attribute, calculation, or aggregate) by name."
  @spec get_field(t(), atom()) :: Ash.Info.Manifest.Field.t() | nil
  def get_field(%__MODULE__{fields: fields}, name), do: Map.get(fields, name)

  @doc "Gets a relationship by name."
  @spec get_relationship(t(), atom()) :: Ash.Info.Manifest.Relationship.t() | nil
  def get_relationship(%__MODULE__{relationships: rels}, name), do: Map.get(rels, name)

  @doc "Gets an identity by name."
  @spec get_identity(t(), atom()) :: %{keys: [atom()]} | nil
  def get_identity(%__MODULE__{identities: identities}, name), do: Map.get(identities, name)

  @doc "Checks if a field or relationship exists by name."
  @spec has_field?(t(), atom()) :: boolean()
  def has_field?(%__MODULE__{fields: fields, relationships: rels}, name) do
    Map.has_key?(fields, name) || Map.has_key?(rels, name)
  end

  @doc """
  Returns all fields as a list, sorted by name.

  Sorting ensures codegen output is deterministic across runs (Erlang map
  iteration order is unstable for maps with more than 32 keys).
  """
  @spec all_fields(t()) :: [Ash.Info.Manifest.Field.t()]
  def all_fields(%__MODULE__{fields: fields}), do: sort_by_name(Map.values(fields))

  @doc "Returns all fields of a given kind (:attribute, :calculation, or :aggregate), sorted by name."
  @spec fields_by_kind(t(), Ash.Info.Manifest.Field.kind()) :: [Ash.Info.Manifest.Field.t()]
  def fields_by_kind(%__MODULE__{fields: fields}, kind) do
    fields |> Map.values() |> Enum.filter(&(&1.kind == kind)) |> sort_by_name()
  end

  @doc "Returns all relationships as a list, sorted by name."
  @spec all_relationships(t()) :: [Ash.Info.Manifest.Relationship.t()]
  def all_relationships(%__MODULE__{relationships: rels}),
    do: sort_by_name(Map.values(rels))

  @doc "Returns all field names (attributes, calculations, aggregates), sorted."
  @spec field_names(t()) :: [atom()]
  def field_names(%__MODULE__{fields: fields}), do: fields |> Map.keys() |> Enum.sort()

  @doc "Returns all relationship names, sorted."
  @spec relationship_names(t()) :: [atom()]
  def relationship_names(%__MODULE__{relationships: rels}),
    do: rels |> Map.keys() |> Enum.sort()

  @doc "Returns relationships whose destination is in the allowed list, sorted by name."
  @spec accessible_relationships(t(), [atom()] | MapSet.t()) :: [
          Ash.Info.Manifest.Relationship.t()
        ]
  def accessible_relationships(%__MODULE__{relationships: rels}, allowed_resources) do
    allowed =
      if is_list(allowed_resources), do: MapSet.new(allowed_resources), else: allowed_resources

    rels
    |> Map.values()
    |> Enum.filter(&MapSet.member?(allowed, &1.destination))
    |> sort_by_name()
  end

  defp sort_by_name(items), do: Enum.sort_by(items, & &1.name)
end
