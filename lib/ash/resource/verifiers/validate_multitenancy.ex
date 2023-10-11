defmodule Ash.Resource.Verifiers.ValidateMultitenancy do
  @moduledoc """
  Ensures that the multitenancy configuration is valid for the given resource
  """
  use Spark.Dsl.Verifier

  alias Spark.Dsl.Verifier

  # sobelow_skip ["DOS.BinToAtom"]
  @impl true
  def verify(dsl_state) do
    strategy = Verifier.get_option(dsl_state, [:multitenancy], :strategy)
    attribute = Verifier.get_option(dsl_state, [:multitenancy], :attribute)
    attributes = Verifier.get_entities(dsl_state, [:attributes])
    resource = Verifier.get_persisted(dsl_state, :module)
    data_layer = Verifier.get_persisted(dsl_state, :data_layer)

    cond do
      strategy == :context && not data_layer.can?(resource, :multitenancy) ->
        {:error,
         Spark.Error.DslError.exception(
           path: [:multitenancy, :strategy],
           message: "Data layer does not support multitenancy"
         )}

      strategy == :attribute && !Enum.any?(attributes, &(&1.name == attribute)) ->
        {:error,
         Spark.Error.DslError.exception(
           path: [:multitenancy, :attribute],
           message: "Attribute #{attribute} used in multitenancy configuration does not exist"
         )}

      true ->
        :ok
    end
  end
end
