defmodule Ash.Resource.Transformers.ManyToManyDestinationAttributeOnJoinResource do
  @moduledoc """
  Guesses the `destination_attribute_on_join_resource` for many to many relationships unless provided.
  """

  use Spark.Dsl.Transformer

  alias Spark.Dsl.Transformer

  def transform(dsl_state) do
    new_dsl_state =
      dsl_state
      |> Transformer.get_entities([:relationships])
      |> Enum.reduce(dsl_state, fn
        %{
          type: type,
          destination: destination,
          destination_attribute_on_join_resource: nil
        } = relationship,
        dsl_state
        when type == :many_to_many ->
          new_relationship = %{
            relationship
            | destination_attribute_on_join_resource: resource_id_field(destination)
          }

          Transformer.replace_entity(
            dsl_state,
            [:relationships],
            new_relationship,
            fn replacing ->
              replacing.name == relationship.name
            end
          )

        _relationship, dsl_state ->
          dsl_state
      end)

    {:ok, new_dsl_state}
  end

  # sobelow_skip ["DOS.StringToAtom"]
  defp resource_id_field(module) do
    module
    |> Module.split()
    |> List.last()
    |> Macro.underscore()
    |> Kernel.<>("_id")
    |> String.to_atom()
  end
end
