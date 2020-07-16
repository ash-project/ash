defmodule Ash.Api.Transformers.ValidateManyToManyJoinAttributes do
  @moduledoc """
  Validates that `join_attributes` on many to many relationships exist on the join resource
  """
  use Ash.Dsl.Transformer

  alias Ash.Dsl.Transformer

  @extension Ash.Api.Dsl

  def transform(_api, dsl) do
    dsl
    |> Transformer.get_entities([:resources], @extension)
    |> Enum.map(& &1.resource)
    |> Enum.each(fn resource ->
      resource
      |> Ash.Resource.relationships()
      |> Enum.filter(&(&1.type == :many_to_many && &1.join_attributes != []))
      |> Enum.each(&validate_relationship/1)
    end)

    {:ok, dsl}
  end

  defp validate_relationship(relationship) do
    through_attributes =
      relationship.through
      |> Ash.Resource.attributes()
      |> Enum.map(& &1.name)

    for join_attribute <- relationship.join_attributes do
      unless join_attribute in through_attributes do
        raise Ash.Error.Dsl.DslError,
          path: [:relationships, relationship.name],
          message:
            "Relationship `#{relationship.name}` expects join_attribute `#{join_attribute}` to be defined on the `through` resource #{
              inspect(relationship.through)
            }"
      end
    end
  end

  def after?(Ash.Api.Transformers.EnsureResourcesCompiled), do: true
  def after?(_), do: false
end
