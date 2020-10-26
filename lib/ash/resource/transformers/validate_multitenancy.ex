defmodule Ash.Resource.Transformers.ValidateMultitenancy do
  @moduledoc """
  Ensures that the multitenancy configuration is valid for the given resource
  """
  use Ash.Dsl.Transformer

  alias Ash.Dsl.Transformer

  # sobelow_skip ["DOS.BinToAtom"]
  def transform(resource, dsl_state) do
    strategy = Transformer.get_option(dsl_state, [:multitenancy], :strategy)
    attribute = Transformer.get_option(dsl_state, [:multitenancy], :attribute)

    cond do
      strategy == :context && not Ash.Resource.data_layer_can?(resource, :multitenancy) ->
        {:error,
         Ash.Error.Dsl.DslError.exception(
           module: __MODULE__,
           path: [:multitenancy, :strategy],
           message: "Data layer does not support multitenancy"
         )}

      strategy == :attribute && is_nil(Ash.Resource.attribute(resource, attribute)) ->
        {:error,
         Ash.Error.Dsl.DslError.exception(
           module: __MODULE__,
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
