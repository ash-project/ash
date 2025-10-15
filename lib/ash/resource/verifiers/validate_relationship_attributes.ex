# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Resource.Verifiers.ValidateRelationshipAttributes do
  @moduledoc """
  Validates that all relationships point to valid fields
  """
  use Spark.Dsl.Verifier
  alias Spark.Dsl.Entity
  alias Spark.Dsl.Verifier

  @impl true
  def verify(dsl) do
    module = Verifier.get_persisted(dsl, :module)

    attribute_names =
      dsl
      |> Ash.Resource.Info.attributes()
      |> Enum.map(& &1.name)

    dsl
    |> Ash.Resource.Info.relationships()
    |> Enum.reject(fn relationship ->
      Map.get(relationship, :manual) || Map.get(relationship, :no_attributes?)
    end)
    |> Enum.filter(& &1.validate_destination_attribute?)
    |> Enum.each(&validate_relationship(&1, attribute_names, module))
  end

  defp validate_relationship(relationship, attribute_names, module) do
    location = Entity.anno(relationship)

    if relationship.source_attribute not in attribute_names do
      raise Spark.Error.DslError,
        module: module,
        location: location,
        path: [:relationships, relationship.name],
        message:
          "Relationship `#{relationship.name}` expects source attribute `#{relationship.source_attribute}` to be defined"
    end

    if Code.ensure_loaded?(relationship.destination) do
      if relationship.type == :many_to_many do
        if Code.ensure_loaded?(relationship.through) do
          through_attributes =
            relationship.through
            |> Ash.Resource.Info.attributes()
            |> Enum.map(& &1.name)

          if relationship.source_attribute_on_join_resource not in through_attributes do
            raise Spark.Error.DslError,
              module: module,
              location: location,
              path: [:relationships, relationship.name],
              message:
                "Relationship `#{relationship.name}` expects source attribute on resource `#{relationship.source_attribute_on_join_resource}` to be defined on #{inspect(relationship.through)}"
          end

          if relationship.destination_attribute_on_join_resource not in through_attributes do
            raise Spark.Error.DslError,
              module: module,
              location: location,
              path: [:relationships, relationship.name],
              message:
                "Relationship `#{relationship.name}` expects destination attribute on join resource `#{relationship.destination_attribute_on_join_resource}` to be defined on #{inspect(relationship.through)}"
          end
        end
      end

      destination_attributes =
        relationship.destination
        |> Ash.Resource.Info.attributes()
        |> Enum.map(& &1.name)

      if relationship.destination_attribute not in destination_attributes do
        raise Spark.Error.DslError,
          module: module,
          location: location,
          path: [:relationships, relationship.name],
          message:
            "Relationship `#{relationship.name}` expects destination field `#{relationship.destination_attribute}` to be defined on #{inspect(relationship.destination)}"
      end
    end
  end
end
