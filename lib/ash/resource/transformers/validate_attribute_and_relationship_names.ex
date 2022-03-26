defmodule Ash.Resource.Transformers.ValidateAttributeAndRelationshipNames do
  @moduledoc """
  Confirms that a resource does not have an attribute and relationship with the same name.
  """
  use Ash.Dsl.Transformer

  alias Ash.Dsl.Transformer
  alias Ash.Error.Dsl.DslError

  def transform(resource, dsl_state) do
    attributes =
      dsl_state
      |> Transformer.get_entities([:attributes])

    relationships =
      dsl_state
      |> Transformer.get_entities([:relationships])

    Enum.concat(attributes, relationships)
    |> Enum.group_by(& &1.name)
    |> Enum.each(fn {name, groups} ->
      unless Enum.count(groups) == 1 do
        raise DslError.exception(
                module: resource,
                message: """
                Attribute and relationship with name `#{name}` are defined in #{inspect(resource)}

                A resource cannot have an attribute and relationship with the same name.
                """
              )
      end
    end)

    {:ok, dsl_state}
  end

  def after?(Ash.Resource.Transformers.BelongsToAttribute), do: true
  def after?(_), do: false
end
