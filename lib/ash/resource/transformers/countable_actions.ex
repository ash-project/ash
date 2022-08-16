defmodule Ash.Resource.Transformers.CountableActions do
  @moduledoc """
  Ensures that countable paginated actions do not exist for resources that are not countable
  """
  use Spark.Dsl.Transformer

  alias Spark.Dsl.Transformer

  def after_compile?, do: true

  # sobelow_skip ["DOS.BinToAtom"]
  def transform(dsl_state) do
    dsl_state
    |> Transformer.get_entities([:actions])
    |> Enum.filter(fn action ->
      action.type == :read && action.pagination && action.pagination.countable
    end)
    |> case do
      [] ->
        {:ok, dsl_state}

      [action | _] ->
        data_layer = Transformer.get_persisted(dsl_state, :data_layer)
        resource = Transformer.get_persisted(dsl_state, :module)

        if data_layer && data_layer.can?(resource, {:query_aggregate, :count}) do
          {:ok, dsl_state}
        else
          {:error,
           Spark.Error.DslError.exception(
             module: resource,
             path: [:actions, action.name],
             message:
               "Action cannot be countable, as the datalayer does not support counting queries"
           )}
        end
    end
  end
end
