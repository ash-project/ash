defmodule Ash.Resource.Transformers.CacheActionInputs do
  @moduledoc "Stores the set of valid input keys for each action"
  use Spark.Dsl.Transformer

  alias Spark.Dsl.Transformer

  def transform(dsl_state) do
    dsl_state
    |> Ash.Resource.Info.actions()
    |> Enum.reject(&(&1.type in [:read, :action]))
    |> Enum.reduce({:ok, dsl_state}, fn action, {:ok, dsl_state} ->
      inputs =
        action.arguments
        |> Enum.map(& &1.name)
        |> Enum.concat(action.accept)
        |> Enum.flat_map(&[&1, to_string(&1)])
        |> MapSet.new()

      {:ok, Transformer.persist(dsl_state, {:action_inputs, action.name}, inputs)}
    end)
  end
end
