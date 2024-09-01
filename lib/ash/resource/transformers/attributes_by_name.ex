defmodule Ash.Resource.Transformers.AttributesByName do
  @moduledoc """
  Persists attribute_names and attributes_by_name.
  """
  use Spark.Dsl.Transformer

  alias Spark.Dsl.Transformer

  def after?(_), do: true

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

    attribute_names = Enum.map(attributes, & &1.name) |> MapSet.new()

    create_attributes_with_static_defaults =
      attributes
      |> Enum.filter(fn attribute ->
        not is_nil(attribute.default) and
          not is_function(attribute.default) and not match?({_, _, _}, attribute.default)
      end)

    create_attributes_with_non_matching_lazy_defaults =
      Enum.filter(attributes, fn attribute ->
        !attribute.match_other_defaults? and
          (is_function(attribute.default) or match?({_, _, _}, attribute.default))
      end)

    create_attributes_with_matching_defaults =
      Enum.filter(attributes, fn attribute ->
        attribute.match_other_defaults? and
          (is_function(attribute.default) or match?({_, _, _}, attribute.default))
      end)

    update_attributes_with_static_defaults =
      attributes
      |> Enum.filter(fn attribute ->
        not is_nil(attribute.update_default) and
          not is_function(attribute.update_default) and
          not match?({_, _, _}, attribute.update_default)
      end)

    update_attributes_with_non_matching_lazy_defaults =
      Enum.filter(attributes, fn attribute ->
        !attribute.match_other_defaults? and
          (is_function(attribute.update_default) or match?({_, _, _}, attribute.update_default))
      end)

    update_attributes_with_matching_defaults =
      Enum.filter(attributes, fn attribute ->
        attribute.match_other_defaults? and
          (is_function(attribute.update_default) or match?({_, _, _}, attribute.update_default))
      end)

    always_selected_attribute_names =
      Enum.filter(attributes, & &1.always_select?)
      |> Enum.map(& &1.name)
      |> MapSet.new()

    {:ok,
     persist(
       dsl_state,
       %{
         attributes_by_name: attributes_by_name,
         attribute_names: attribute_names,
         create_attributes_with_static_defaults: create_attributes_with_static_defaults,
         create_attributes_with_non_matching_lazy_defaults:
           create_attributes_with_non_matching_lazy_defaults,
         create_attributes_with_matching_defaults: create_attributes_with_matching_defaults,
         update_attributes_with_static_defaults: update_attributes_with_static_defaults,
         update_attributes_with_non_matching_lazy_defaults:
           update_attributes_with_non_matching_lazy_defaults,
         update_attributes_with_matching_defaults: update_attributes_with_matching_defaults,
         always_selected_attribute_names: always_selected_attribute_names
       }
     )}
  end

  defp persist(dsl, map) do
    Enum.reduce(map, dsl, fn {key, value}, dsl ->
      Transformer.persist(dsl, key, value)
    end)
  end
end
