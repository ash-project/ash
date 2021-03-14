defmodule Ash.Resource.Transformers.ReplaceTimestamps do
  @moduledoc "Replaces a single `timestamps()` attribute with `inserted_at` and `updated_at` timestamps."
  use Ash.Dsl.Transformer

  alias Ash.Dsl.Transformer

  def transform(_resource, dsl_state) do
    attributes =
      dsl_state
      |> Transformer.get_entities([:attributes])
      |> Enum.reduce([], fn
        %{name: :__timestamps__}, attrs ->
          attrs ++ timestamp_attributes()

        attr, attrs ->
          attrs ++ [attr]
      end)

    new_dsl_state = put_in(dsl_state, [[:attributes], :entities], attributes)

    {:ok, new_dsl_state}
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
