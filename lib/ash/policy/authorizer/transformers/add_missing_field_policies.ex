defmodule Ash.Policy.Authorizer.Transformers.AddMissingFieldPolicies do
  @moduledoc "Adds field policies for any missing fields"
  use Spark.Dsl.Transformer
  import Spark.Dsl.Builder

  alias Spark.Dsl.Transformer

  def after?(Ash.Policy.Authorizer.Transformers.CacheFieldPolicyExpressions), do: false
  def after?(_), do: true

  def transform(dsl) do
    if Enum.empty?(Ash.Policy.Info.field_policies(dsl)) do
      {:ok, dsl}
    else
      exclude_private_fields? =
        case Ash.Policy.Info.private_fields_policy(dsl) do
          :include -> false
          :show -> true
          :hide -> true
        end

      non_pkey_fields =
        dsl
        |> Ash.Resource.Info.fields([:aggregates, :calculations, :attributes])
        |> Enum.reject(fn
          %{public?: false} when exclude_private_fields? ->
            true

          %{primary_key?: true} ->
            true

          _ ->
            false
        end)
        |> Enum.map(& &1.name)

      dsl
      |> replace_asterisk(non_pkey_fields)
      |> ensure_field_coverage(non_pkey_fields)
    end
  end

  defbuilderp replace_asterisk(dsl, non_pkey_fields) do
    field_policies = Ash.Policy.Info.field_policies(dsl)

    Enum.reduce(field_policies, dsl, fn policy, dsl ->
      if :* in policy.fields do
        fields =
          policy.fields
          |> Enum.reject(&(&1 == :*))
          |> Enum.concat(non_pkey_fields)

        Transformer.replace_entity(dsl, [:field_policies], %{policy | fields: fields})
      else
        dsl
      end
    end)
  end

  defbuilderp ensure_field_coverage(dsl, non_pkey_fields) do
    field_policies = Ash.Policy.Info.field_policies(dsl)

    fields =
      field_policies
      |> Enum.flat_map(& &1.fields)
      |> Enum.uniq()

    invalid_fields = Enum.filter(fields, &(&1 not in non_pkey_fields))

    missing_fields = Enum.filter(non_pkey_fields, &(&1 not in fields))

    module = Transformer.get_persisted(dsl, :module)

    unless Enum.empty?(invalid_fields) do
      raise Spark.Error.DslError,
        module: module,
        path: [:field_policies],
        message: """
        Invalid field reference(s) in field policy: #{inspect(invalid_fields)}

        Only non primary-key, public attributes, calculations and aggregates are supported.
        """
    end

    unless Enum.empty?(missing_fields) do
      raise Spark.Error.DslError,
        module: module,
        path: [:field_policies],
        message: """
        Missing field reference(s) in field policies: #{inspect(missing_fields)}

        If any field policies are present, *all* public, non-primary key fields must be accounted for.

        To create a catch-all field policy that allows any fields that aren't covered
        by other policies, use:

            field_policy :* do
              authorize_if always()
            end

        Keep in mind that all policies relevant to a given field must pass, so this will
        not override other field policies.
        """
    end

    {:ok, dsl}
  end
end
