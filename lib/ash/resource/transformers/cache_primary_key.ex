defmodule Ash.Resource.Transformers.CachePrimaryKey do
  @moduledoc "Validates the primary key of a resource, and caches it in `:persistent_term` for fast access"
  use Ash.Dsl.Transformer

  alias Ash.Dsl.Transformer
  alias Ash.Error.Dsl.DslError

  @extension Ash.Dsl

  def transform(resource, dsl_state) do
    primary_key =
      dsl_state
      |> Transformer.get_entities([:attributes], @extension)
      |> Enum.filter(& &1.primary_key?)
      |> Enum.map(& &1.name)

    case primary_key do
      [] ->
        {:error,
         DslError.exception(message: "Resources without a primary key are not yet supported")}

      [field] ->
        :persistent_term.put({resource, :primary_key}, [field])

        {:ok, dsl_state}

      fields ->
        if Ash.Resource.data_layer(resource) &&
             Ash.Resource.data_layer_can?(resource, :composite_primary_key) do
          :persistent_term.put({resource, :primary_key}, fields)

          {:ok, dsl_state}
        else
          {:error,
           DslError.exception(message: "Data layer does not support composite primary keys")}
        end
    end
  end

  def after?(Ash.Resource.Transformers.BelongsToAttribute), do: true

  def after?(_), do: false
end
