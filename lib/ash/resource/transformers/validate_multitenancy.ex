defmodule Ash.Resource.Transformers.ValidateMultitenancy do
  @moduledoc """
  Ensures that the multitenancy configuration is valid for the given resource
  """
  use Spark.Dsl.Transformer

  alias Spark.Dsl.Transformer

  def after_compile?, do: true

  # sobelow_skip ["DOS.BinToAtom"]
  def transform(dsl_state) do
    strategy = Transformer.get_option(dsl_state, [:multitenancy], :strategy)
    attribute = Transformer.get_option(dsl_state, [:multitenancy], :attribute)
    attributes = Transformer.get_entities(dsl_state, [:attributes])
    resource = Transformer.get_persisted(dsl_state, :module)
    data_layer = Transformer.get_persisted(dsl_state, :data_layer)

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
        {:ok, dsl_state}
    end
  end
end
