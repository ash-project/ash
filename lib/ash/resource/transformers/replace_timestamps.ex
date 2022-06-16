defmodule Ash.Resource.Transformers.ReplaceTimestamps do
  @moduledoc "Replaces a single `timestamps()` attribute with `inserted_at` and `updated_at` timestamps."
  use Ash.Dsl.Transformer

  alias Ash.Dsl.Transformer

  def transform(_resource, dsl_state) do
    attributes =
      dsl_state
      |> Transformer.get_entities([:attributes])
      |> Enum.flat_map(fn
        %{name: :__timestamps__} ->
          timestamp_attributes()

        _ ->
          []
      end)

    {:ok,
     Enum.reduce(attributes, dsl_state, fn attr, dsl_state ->
       dsl_state
       |> Transformer.add_entity([:attributes], attr)
       |> Transformer.remove_entity([:attributes], fn attr ->
         attr.name == :__timestamps__
       end)
     end)}
  end

  defp timestamp_attributes do
    %{
      inserted_at: Ash.Resource.Attribute.create_timestamp_schema(),
      updated_at: Ash.Resource.Attribute.update_timestamp_schema()
    }
    |> Enum.map(&build_attribute/1)
    |> Enum.map(fn {:ok, attr} -> attr end)
  end

  defp build_attribute({name, schema}) do
    params = %{
      target: Ash.Resource.Attribute,
      schema: schema,
      auto_set_fields: [name: name],
      transform: nil
    }

    Ash.Dsl.Entity.build(params, [], [])
  end
end
