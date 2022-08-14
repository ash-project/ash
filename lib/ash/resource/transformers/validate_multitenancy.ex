defmodule Ash.Resource.Transformers.ValidateMultitenancy do
  @moduledoc """
  Ensures that the multitenancy configuration is valid for the given resource
  """
  use Spark.Dsl.Transformer

  alias Spark.Dsl.Transformer

  # sobelow_skip ["DOS.BinToAtom"]
  def transform(resource, dsl_state) do
    strategy = Transformer.get_option(dsl_state, [:multitenancy], :strategy)
    attribute = Transformer.get_option(dsl_state, [:multitenancy], :attribute)

    cond do
      strategy == :context && not Ash.DataLayer.data_layer_can?(resource, :multitenancy) ->
        {:error,
         Spark.Error.DslError.exception(
           module: resource,
           path: [:multitenancy, :strategy],
           message: "Data layer does not support multitenancy"
         )}

      strategy == :attribute && is_nil(Ash.Resource.Info.attribute(resource, attribute)) ->
        {:error,
         Spark.Error.DslError.exception(
           module: resource,
           path: [:multitenancy, :attribute],
           message: "Attribute #{attribute} used in multitenancy configuration does not exist"
         )}

      true ->
        {:ok, dsl_state}
    end
  end

  def after?(Ash.Resource.Transformers.BelongsToAttribute), do: true
  def after?(_), do: false
end
