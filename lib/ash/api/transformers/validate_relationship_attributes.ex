defmodule Ash.Api.Transformers.ValidateRelationshipAttributes do
  @moduledoc """
  Validates that all relationships point to valid fields
  """
  use Ash.Dsl.Transformer

  alias Ash.Dsl.Transformer

  def transform(_api, dsl) do
    dsl
    |> Transformer.get_entities([:resources])
    |> Enum.filter(& &1.warn_on_compile_failure?)
    |> Enum.map(& &1.resource)
    |> Enum.each(fn resource ->
      attribute_names =
        resource
        |> Ash.Resource.Info.attributes()
        |> Enum.map(& &1.name)

      resource
      |> Ash.Resource.Info.relationships()
      |> Enum.filter(& &1.validate_destination_field?)
      |> Enum.each(&validate_relationship(&1, attribute_names))
    end)

    {:ok, dsl}
  end

  defp validate_relationship(relationship, attribute_names) do
    unless relationship.source_field in attribute_names do
      raise Ash.Error.Dsl.DslError,
        module: __MODULE__,
        path: [:relationships, relationship.name],
        message:
          "Relationship `#{relationship.name}` expects source field `#{relationship.source_field}` to be defined"
    end

    if relationship.type == :many_to_many do
      through_attributes =
        relationship.through
        |> Ash.Resource.Info.attributes()
        |> Enum.map(& &1.name)

      unless relationship.source_field_on_join_table in through_attributes do
        raise Ash.Error.Dsl.DslError,
          module: __MODULE__,
          path: [:relationships, relationship.name],
          message:
            "Relationship `#{relationship.name}` expects source field on join table `#{
              relationship.source_field_on_join_table
            }` to be defined on #{inspect(relationship.through)}"
      end

      unless relationship.destination_field_on_join_table in through_attributes do
        raise Ash.Error.Dsl.DslError,
          module: __MODULE__,
          path: [:relationships, relationship.name],
          message:
            "Relationship `#{relationship.name}` expects destination field on join table `#{
              relationship.destination_field_on_join_table
            }` to be defined on #{inspect(relationship.through)}"
      end
    end

    destination_attributes =
      relationship.destination
      |> Ash.Resource.Info.attributes()
      |> Enum.map(& &1.name)

    unless relationship.destination_field in destination_attributes do
      raise Ash.Error.Dsl.DslError,
        module: __MODULE__,
        path: [:relationships, relationship.name],
        message:
          "Relationship `#{relationship.name}` expects destination field `#{
            relationship.destination_field
          }` to be defined on #{inspect(relationship.destination)}"
    end
  end

  def after?(Ash.Api.Transformers.EnsureResourcesCompiled), do: true
  def after?(_), do: false
end
