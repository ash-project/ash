defmodule Ash.Resource.Transformers.CachePrimaryKey do
  @moduledoc "Validates and caches the primary key of a resource"
  use Spark.Dsl.Transformer

  alias Spark.Dsl.Transformer
  alias Spark.Error.DslError

  def transform(dsl_state) do
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
        {:error, DslError.exception(message: "Primary keys must not be allowed to be nil")}

      [] ->
        {:error,
         DslError.exception(message: "Resources without a primary key are not yet supported")}

      [field] ->
        dsl_state = Transformer.persist(dsl_state, :primary_key, [field])

        {:ok, dsl_state}

      fields ->
        dsl_state = Transformer.persist(dsl_state, :primary_key, fields)

        {:ok, dsl_state}
    end
  end

  def after?(Ash.Resource.Transformers.BelongsToAttribute), do: true
  def after?(Ash.Resource.Transformers.DefaultPrimaryKey), do: true

  def after?(_), do: false
end
