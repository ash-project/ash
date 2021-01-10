defmodule Ash.Resource.Transformers.CachePrimaryKey do
  @moduledoc "Validates and caches the primary key of a resource"
  use Ash.Dsl.Transformer

  alias Ash.Dsl.Transformer
  alias Ash.Error.Dsl.DslError

  def transform(resource, dsl_state) do
    primary_key_attribute =
      dsl_state
      |> Transformer.get_entities([:attributes])
      |> Enum.filter(& &1.primary_key?)

    pk_allows_nil? = Enum.count(primary_key_attribute, & &1.allow_nil?) > 0

    primary_key =
      unless pk_allows_nil? do
        Enum.map(primary_key_attribute, & &1.name)
      else
        :error_pk_allows_nil
      end

    case primary_key do
      :error_pk_allows_nil ->
        {:error,
         DslError.exception(
           module: __MODULE__,
           message: "Primary keys must not be allowed to be nil"
         )}

      [] ->
        {:error,
         DslError.exception(
           module: __MODULE__,
           message: "Resources without a primary key are not yet supported"
         )}

      [field] ->
        dsl_state = Transformer.persist(dsl_state, :primary_key, [field])
        {:ok, dsl_state}

      fields ->
        if Ash.Resource.data_layer(resource) &&
             Ash.Resource.data_layer_can?(resource, :composite_primary_key) do
          dsl_state = Transformer.persist(dsl_state, :primary_key, fields)

          {:ok, dsl_state}
        else
          {:error,
           DslError.exception(
             module: __MODULE__,
             message: "Data layer does not support composite primary keys"
           )}
        end
    end
  end

  def after?(Ash.Resource.Transformers.BelongsToAttribute), do: true

  def after?(_), do: false
end
