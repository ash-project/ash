defmodule Ash.Resource.Transformers.ValidateRelationshipAttributes do
  @moduledoc """
  Sets the default `source_field` for belongs_to attributes
  """
  use Ash.Dsl.Transformer

  alias Ash.Dsl.Transformer

  @extension Ash.Dsl

  def after_compile?, do: true

  # sobelow_skip ["DOS.BinToAtom"]
  def transform(_resource, dsl_state) do
    attribute_names =
      dsl_state
      |> Transformer.get_entities([:attributes], @extension)
      |> Enum.map(& &1.name)

    dsl_state
    |> Transformer.get_entities([:relationships], @extension)
    |> Enum.each(&validate_relationship(&1, attribute_names))

    {:ok, dsl_state}
  end

  defp validate_relationship(relationship, attribute_names) do
    case Code.ensure_compiled(relationship.destination) do
      {:error, _} ->
        # If the resource doesn't exist/can't be compiled
        # a different error will catch that. This failure
        # most likely implies that we are compiling resources
        # in the same file as eachother.
        :ok

      {:module, _module} ->
        unless relationship.source_field in attribute_names do
          raise Ash.Error.ResourceDslError,
            path: [:relationships, relationship.name],
            message:
              "Relationship `#{relationship.name}` expects source field `#{
                relationship.source_field
              }` to be defined"
        end

        destination_attributes =
          relationship.destination
          |> Ash.attributes()
          |> Enum.map(& &1.name)

        unless relationship.destination_field in destination_attributes do
          raise Ash.Error.ResourceDslError,
            path: [:relationships, relationship.name],
            message:
              "Relationship `#{relationship.name}` expects destination field `#{
                relationship.destination_field
              }` to be defined on #{inspect(relationship.destination)}"
        end
    end
  end
end
