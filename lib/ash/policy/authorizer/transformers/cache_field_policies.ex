defmodule Ash.Policy.Authorizer.Transformers.CacheFieldPolicies do
  use Spark.Dsl.Transformer
  alias Spark.Dsl.Transformer

  def after?(_), do: true

  def transform(dsl) do
    fields_to_field_policies =
      dsl
      |> Ash.Policy.Info.field_policies()
      |> Enum.reduce(%{}, fn field_policy, acc ->
        field_policy.fields
        |> Enum.reduce(acc, fn field, acc ->
          Map.update(acc, field, [field_policy], &[field_policy | &1])
        end)
      end)

    {:ok, Transformer.persist(dsl, :fields_to_field_policies, fields_to_field_policies)}
  end
end
