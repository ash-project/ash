defmodule Ash.Resource.Transformers.CachePrimaryKey do
  @moduledoc "Validates the primary key of a resource, and caches it in `:persistent_term` for fast access"
  use Ash.Dsl.Transformer

  alias Ash.Dsl.Transformer

  @extension Ash.Dsl

  def transform(resource, dsl_state) do
    primary_key =
      dsl_state
      |> Transformer.get_entities([:attributes], @extension)
      |> Enum.filter(& &1.primary_key?)
      |> Enum.map(& &1.name)

    case primary_key do
      [] ->
        {:error, "Resources without a primary key are not yet supported"}

      [field] ->
        Transformer.persist_to_runtime(resource, {resource, :primary_key}, [field])

        {:ok, dsl_state}

      fields ->
        if Ash.data_layer(resource) && Ash.data_layer_can?(resource, :composite_primary_key) do
          Transformer.persist_to_runtime(resource, {resource, :primary_key}, fields)

          {:ok, dsl_state}
        else
          {:error, "Data layer does not support composite primary keys"}
        end
    end
  end

  def after?(Ash.Resource.Transformers.BelongsToAttribute), do: true

  def after?(_), do: false
end
