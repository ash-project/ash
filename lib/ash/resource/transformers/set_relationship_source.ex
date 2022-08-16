defmodule Ash.Resource.Transformers.SetRelationshipSource do
  @moduledoc "Sets the `source` key on relationships to be the resource they were defined on"
  use Spark.Dsl.Transformer

  alias Spark.Dsl.Transformer

  def transform(dsl_state) do
    dsl_state
    |> Transformer.get_entities([:relationships])
    |> Enum.reduce({:ok, dsl_state}, fn relationship, {:ok, dsl_state} ->
      new_relationship = %{relationship | source: Transformer.get_persisted(dsl_state, :module)}

      new_dsl_state =
        Transformer.replace_entity(
          dsl_state,
          [:relationships],
          new_relationship,
          fn replacing ->
            replacing.name == relationship.name
          end
        )

      {:ok, new_dsl_state}
    end)
  end
end
