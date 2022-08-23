defmodule Ash.Resource.Transformers.CachePrimaryKey do
  @moduledoc "Validates and caches the primary key of a resource"
  use Spark.Dsl.Transformer

  alias Spark.Dsl.Transformer
  alias Spark.Error.DslError

  def transform(resource, dsl_state) do
    primary_key_attribute =
      dsl_state
      |> Transformer.get_entities([:attributes])
      |> Enum.filter(& &1.primary_key?)

    pk_allows_nil? = Enum.any?(primary_key_attribute, & &1.allow_nil?)

    primary_key =
      if pk_allows_nil? == false do
        Enum.map(primary_key_attribute, & &1.name)
      else
        :error_pk_allows_nil
      end

    case primary_key do
      :error_pk_allows_nil ->
        {:error,
         DslError.exception(
           module: resource,
           message: "Primary keys must not be allowed to be nil"
         )}

      [] ->
        {:error,
         DslError.exception(
           module: resource,
           message: "Resources without a primary key are not yet supported"
         )}

      [field] ->
        dsl_state = Transformer.persist(dsl_state, :primary_key, [field])

        {:ok, dsl_state}

      fields ->
        if Ash.DataLayer.data_layer(resource) &&
             Ash.DataLayer.data_layer_can?(resource, :composite_primary_key) do
          dsl_state = Transformer.persist(dsl_state, :primary_key, fields)

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

  def after?(Ash.Resource.Transformers.BelongsToAttribute), do: true
  def after?(Ash.Resource.Transformers.DefaultPrimaryKey), do: true

  def after?(_), do: false
end
