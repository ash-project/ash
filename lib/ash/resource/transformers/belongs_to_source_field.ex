defmodule Ash.Resource.Transformers.BelongsToSourceField do
  @moduledoc """
  Sets the default `source_attribute` for belongs_to attributes
  """
  use Spark.Dsl.Transformer

  alias Spark.Dsl.Transformer

  # sobelow_skip ["DOS.BinToAtom"]
  def transform(dsl_state) do
    dsl_state
    |> Transformer.get_entities([:relationships])
    |> Enum.filter(&(&1.type == :belongs_to))
    |> Enum.reject(& &1.source_attribute)
    |> Enum.reduce({:ok, dsl_state}, fn relationship, {:ok, dsl_state} ->
      new_dsl_state =
        Transformer.replace_entity(
          dsl_state,
          [:relationships],
          %{relationship | source_attribute: :"#{relationship.name}_id"},
          &(&1.name == relationship.name)
        )

      {:ok, new_dsl_state}
    end)
  end
end
