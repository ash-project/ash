defmodule Ash.Resource.Transformers.HasManyDestinationField do
  @moduledoc "Guesses the `destination_field` for has many relationships unless provided"
  use Ash.Dsl.Transformer

  alias Ash.Dsl.Transformer

  def transform(resource, dsl_state) do
    new_dsl_state =
      dsl_state
      |> Transformer.get_entities([:relationships])
      |> Enum.reduce(dsl_state, fn
        %{type: :has_many, destination_field: nil} = relationship, dsl_state ->
          new_relationship = %{relationship | destination_field: resource_id_field(resource)}

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

  # sobelow_skip ["DOS.BinToAtom"]
  defp resource_id_field(resource) do
    resource
    |> Module.split()
    |> List.last()
    |> Macro.underscore()
    |> Kernel.<>("_id")
    |> String.to_atom()
  end
end
