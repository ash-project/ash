defmodule Ash.Resource.Transformers.ValidatePrimaryKey do
  @moduledoc "Validates and caches the primary key of a resource"
  use Spark.Dsl.Transformer

  alias Spark.Dsl.Transformer
  alias Spark.Error.DslError

  def after_compile?, do: true

  def transform(dsl_state) do
    primary_key = Transformer.get_persisted(dsl_state, :primary_key)

    case primary_key do
      [] ->
        {:ok, dsl_state}

      [_] ->
        {:ok, dsl_state}

      _ ->
        data_layer = Transformer.get_persisted(dsl_state, :data_layer)
        resource = Transformer.get_persisted(dsl_state, :module)

        if data_layer && data_layer.can?(resource, :composite_primary_key) do
          {:ok, dsl_state}
        else
          {:error,
           DslError.exception(
             module: resource,
             message: "Data layer does not support composite primary keys"
           )}
        end
    end
  end
end
