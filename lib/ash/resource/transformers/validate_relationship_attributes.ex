defmodule Ash.Resource.Transformers.ValidateRelationshipAttributes do
  @moduledoc """
  Validates that all relationships point to valid fields
  """
  use Ash.Dsl.Transformer

  @impl true
  def after_compile?, do: true

  @impl true
  def transform(resource, dsl) do
    attribute_names =
      resource
      |> Ash.Resource.Info.attributes()
      |> Enum.map(& &1.name)

    resource
    |> Ash.Resource.Info.relationships()
    |> Enum.reject(fn relationship ->
      Map.get(relationship, :manual) || Map.get(relationship, :no_fields?)
    end)
    |> Enum.filter(& &1.validate_destination_field?)
    |> Enum.each(&validate_relationship(&1, attribute_names, resource))

    {:ok, dsl}
  end

  defp validate_relationship(relationship, attribute_names, resource) do
    unless relationship.source_field in attribute_names do
      raise Ash.Error.Dsl.DslError,
        module: resource,
        path: [:relationships, relationship.name],
        message:
          "Relationship `#{relationship.name}` expects source field `#{relationship.source_field}` to be defined"
    end

    if Code.ensure_loaded?(relationship.destination) do
      if relationship.type == :many_to_many do
        if Code.ensure_loaded?(relationship.through) do
          through_attributes =
            relationship.through
            |> Ash.Resource.Info.attributes()
            |> Enum.map(& &1.name)

          unless relationship.source_field_on_join_table in through_attributes do
            raise Ash.Error.Dsl.DslError,
              module: resource,
              path: [:relationships, relationship.name],
              message:
                "Relationship `#{relationship.name}` expects source field on join table `#{relationship.source_field_on_join_table}` to be defined on #{inspect(relationship.through)}"
          end

          unless relationship.destination_field_on_join_table in through_attributes do
            raise Ash.Error.Dsl.DslError,
              module: resource,
              path: [:relationships, relationship.name],
              message:
                "Relationship `#{relationship.name}` expects destination field on join table `#{relationship.destination_field_on_join_table}` to be defined on #{inspect(relationship.through)}"
          end
        end
      end

      destination_attributes =
        relationship.destination
        |> Ash.Resource.Info.attributes()
        |> Enum.map(& &1.name)

      unless relationship.destination_field in destination_attributes do
        raise Ash.Error.Dsl.DslError,
          module: resource,
          path: [:relationships, relationship.name],
          message:
            "Relationship `#{relationship.name}` expects destination field `#{relationship.destination_field}` to be defined on #{inspect(relationship.destination)}"
      end
    end
  end
end
