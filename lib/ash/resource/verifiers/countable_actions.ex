defmodule Ash.Resource.Verifiers.CountableActions do
  @moduledoc """
  Ensures that countable paginated actions do not exist for resources that are not countable
  """
  use Spark.Dsl.Verifier

  alias Spark.Dsl.Verifier

  # sobelow_skip ["DOS.BinToAtom"]
  def verify(dsl_state) do
    dsl_state
    |> Verifier.get_entities([:actions])
    |> Enum.filter(fn action ->
      action.type == :read && action.pagination && action.pagination.countable
    end)
    |> case do
      [] ->
        :ok

      [action | _] ->
        data_layer = Verifier.get_persisted(dsl_state, :data_layer)
        resource = Verifier.get_persisted(dsl_state, :module)

        if data_layer && data_layer.can?(resource, {:query_aggregate, :count}) do
          :ok
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
