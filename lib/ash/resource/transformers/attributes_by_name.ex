defmodule Ash.Resource.Transformers.AttributesByName do
  @moduledoc """
  Persists attribute_names and attributes_by_name.
  """
  use Spark.Dsl.Transformer

  alias Spark.Dsl.Transformer

  def transform(dsl_state) do
    attributes =
      Ash.Resource.Info.attributes(dsl_state)

    attributes_by_name =
      attributes
      |> Enum.reduce(%{}, fn %{name: name} = attr, acc ->
        acc
        |> Map.put(name, attr)
        |> Map.put(to_string(name), attr)
      end)

    attribute_names = Enum.map(attributes, & &1.name)

    {:ok,
     dsl_state
     |> Transformer.persist(:attributes_by_name, attributes_by_name)
     |> Transformer.persist(:attribute_names, attribute_names)}
  end
end
