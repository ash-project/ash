# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Policy.Authorizer.Transformers.CachePolicyExpressions do
  @moduledoc """
  Builds and simplifies the policy expressions at compile time.

  Persists, for the resource's policies, the simplified boolean expression as a
  whole and one specialized for each action (with the action, action type and
  private action checks already resolved), as well as one expression per
  distinct set of field policies. At runtime the authorizer starts from these
  instead of rebuilding and simplifying the same expression for every request.

  The resource's expressions are persisted as the open fold
  `{one_condition_matches, all_policies_match}`. For a domain, a template of
  its policies over that fold is persisted instead, and composed with the
  resource's at runtime (the domain's policies come before the resource's).

  This is a persister, so it runs once every transformer (actions, policies,
  field policies) has finished.
  """
  use Spark.Dsl.Transformer

  alias Ash.Policy.Policy
  alias Spark.Dsl.Transformer

  def transform(dsl) do
    module = Transformer.get_persisted(dsl, :module)
    policies = Ash.Policy.Info.policies(nil, dsl)

    if Ash.Domain.Dsl in Transformer.get_persisted(dsl, :spark_extensions, []) do
      {:ok,
       Transformer.persist(dsl, :domain_policy_expression, %{
         policies: policies,
         template: Policy.domain_template(policies, module)
       })}
    else
      persist_resource_expressions(dsl, module, policies)
    end
  end

  defp persist_resource_expressions(dsl, module, policies) do
    by_action =
      dsl
      |> Ash.Resource.Info.actions()
      |> Map.new(fn action ->
        {action.name, Policy.static_cache_entry(policies, module, action)}
      end)

    policy_expressions = %{
      policies: policies,
      overall: Policy.static_cache_entry(policies, module, nil),
      by_action: by_action
    }

    field_policy_expressions =
      dsl
      |> Ash.Policy.Info.field_policies()
      |> Enum.reduce(%{}, fn field_policy, acc ->
        Enum.reduce(field_policy.fields, acc, fn field, acc ->
          Map.update(acc, field, [field_policy], &(&1 ++ [field_policy]))
        end)
      end)
      |> Map.values()
      |> Enum.uniq()
      |> Map.new(fn field_policies ->
        {field_policies, Policy.static_expression(field_policies, module, nil)}
      end)

    {:ok,
     dsl
     |> Transformer.persist(:policy_expression_cache, policy_expressions)
     |> Transformer.persist(:field_policy_expressions, field_policy_expressions)}
  end
end
