defmodule Ash.Resource.Transformers.ManyToManySourceAttributeOnJoinResource do
  @moduledoc """
  Guesses the `source_attribute_on_join_resource` for many to many relationships unless provided.
  """

  use Spark.Dsl.Transformer

  alias Spark.Dsl.Transformer

  def transform(dsl_state) do
    new_dsl_state =
      dsl_state
      |> Transformer.get_entities([:relationships])
      |> Enum.reduce(dsl_state, fn
        %{type: type, source_attribute_on_join_resource: nil} = relationship, dsl_state
        when type in [:many_to_many] ->
          new_relationship = %{
            relationship
            | source_attribute_on_join_resource: resource_id_field(dsl_state)
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
  defp resource_id_field(dsl_state) do
    dsl_state
    |> Transformer.get_persisted(:module)
    |> Module.split()
    |> List.last()
    |> Macro.underscore()
    |> Kernel.<>("_id")
    |> String.to_atom()
  end
end
