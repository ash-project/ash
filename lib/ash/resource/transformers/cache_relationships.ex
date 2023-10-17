defmodule Ash.Resource.Transformers.CacheRelationships do
  @moduledoc """
  Persists commonly used relationship information.
  """
  use Spark.Dsl.Transformer

  alias Spark.Dsl.Transformer

  def after?(_), do: true

  def transform(dsl_state) do
    relationships =
      Ash.Resource.Info.relationships(dsl_state)

    relationships_by_name =
      relationships
      |> Enum.reduce(%{}, fn %{name: name} = attr, acc ->
        acc
        |> Map.put(name, attr)
        |> Map.put(to_string(name), attr)
      end)

    relationship_names = Enum.map(relationships, & &1.name)

    required_belongs_to_relationships =
      Enum.filter(relationships, &(&1.type == :belongs_to && !&1.allow_nil?))

    {:ok,
     persist(
       dsl_state,
       %{
         relationship_names: relationship_names,
         relationships_by_name: relationships_by_name,
         required_belongs_to_relationships: required_belongs_to_relationships
       }
     )}
  end

  defp persist(dsl, map) do
    Enum.reduce(map, dsl, fn {key, value}, dsl ->
      Transformer.persist(dsl, key, value)
    end)
  end
end
