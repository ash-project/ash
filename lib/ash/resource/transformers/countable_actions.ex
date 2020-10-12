defmodule Ash.Resource.Transformers.CountableActions do
  @moduledoc """
  Ensures that countable paginated actions do not exist for resources that are not countable
  """
  use Ash.Dsl.Transformer

  alias Ash.Dsl.Transformer

  # sobelow_skip ["DOS.BinToAtom"]
  def transform(resource, dsl_state) do
    dsl_state
    |> Transformer.get_entities([:actions])
    |> Enum.filter(&(&1.type == :read))
    |> Enum.filter(& &1.pagination)
    |> Enum.filter(& &1.pagination.countable)
    |> case do
      [] ->
        {:ok, dsl_state}

      [action | _] ->
        if Ash.Resource.data_layer_can?(resource, {:query_aggregate, :count}) do
          {:ok, dsl_state}
        else
          {:error,
           Ash.Error.Dsl.DslError.exception(
             module: __MODULE__,
             path: [:actions, action.name],
             message:
               "Action cannot be countable, as the datalayer does not support counting queries"
           )}
        end
    end
  end
end
