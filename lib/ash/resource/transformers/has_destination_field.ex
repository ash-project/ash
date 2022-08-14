defmodule Ash.Resource.Transformers.HasDestinationField do
  @moduledoc "Guesses the `destination_field` for has many and has one relationships unless provided"
  use Spark.Dsl.Transformer

  alias Spark.Dsl.Transformer

  def transform(resource, dsl_state) do
    new_dsl_state =
      dsl_state
      |> Transformer.get_entities([:relationships])
      |> Enum.reduce(dsl_state, fn
        %{type: type, destination_field: nil} = relationship, dsl_state
        when type in [:has_many, :has_one] ->
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

  # sobelow_skip ["DOS.StringToAtom"]
  defp resource_id_field(resource) do
    resource
    |> Module.split()
    |> List.last()
    |> Macro.underscore()
    |> Kernel.<>("_id")
    |> String.to_atom()
  end
end
