defmodule Ash.Resource.Transformers.SetRelationshipSource do
  @moduledoc "Sets the `source` key on relationships to be the resource they were defined on"
  use Ash.Dsl.Transformer

  alias Ash.Dsl.Transformer

  def transform(resource, dsl_state) do
    dsl_state
    |> Transformer.get_entities([:relationships])
    |> Enum.reduce({:ok, dsl_state}, fn relationship, {:ok, dsl_state} ->
      new_relationship = %{relationship | source: resource}

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
