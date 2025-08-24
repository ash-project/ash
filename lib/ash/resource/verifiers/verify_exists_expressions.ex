defmodule Ash.Resource.Verifiers.VerifyExistsExpressions do
  @moduledoc """
  Raises an error if an exists expression references an invalid relationship or resource.
  """
  use Spark.Dsl.Verifier

  alias Spark.Error.DslError

  @impl true
  def verify(dsl) do
    module = Spark.Dsl.Verifier.get_persisted(dsl, :module)

    dsl
    |> Ash.Resource.Info.actions()
    |> Enum.each(fn action ->
      verify_action_filter!(module, action)
    end)

    dsl
    |> get_policies()
    |> Enum.each(fn policy ->
      verify_policy!(module, policy)
    end)

    :ok
  end

  defp get_policies(dsl) do
    Spark.Dsl.Extension.get_entities(dsl, [:policies])
    |> flatten_policy_groups()
  end

  defp flatten_policy_groups(policies) do
    Enum.flat_map(policies, fn
      %Ash.Policy.Policy{} = policy ->
        [policy]

      %Ash.Policy.PolicyGroup{policies: policies} ->
        flatten_policy_groups(policies)
    end)
  end

  defp verify_action_filter!(module, %{filter: filter} = action) when not is_nil(filter) do
    Ash.Expr.walk_template(filter, fn
      %Ash.Query.Exists{} = exists ->
        verify_exists_expression!(module, exists, [:actions, action.name, :filter])
        exists

      other ->
        other
    end)
  end

  defp verify_action_filter!(_module, _action), do: :ok

  defp verify_policy!(module, policy) do
    verify_policy_conditions!(module, policy.condition, [:policies, :policy, :condition])

    Enum.each(policy.policies || [], fn check ->
      verify_policy_check!(module, check, [:policies, :policy])
    end)
  end

  defp verify_policy_conditions!(module, conditions, path) when is_list(conditions) do
    Enum.each(conditions, fn condition ->
      verify_policy_condition!(module, condition, path)
    end)
  end

  defp verify_policy_conditions!(_module, _condition, _path), do: :ok

  defp verify_policy_condition!(module, {Ash.Policy.Check.Expression, opts}, path) do
    expr = Keyword.get(opts, :expr)

    if expr do
      verify_expression_for_exists!(module, expr, path)
    end
  end

  defp verify_policy_condition!(_module, _condition, _path), do: :ok

  defp verify_policy_check!(
         module,
         %{check_module: Ash.Policy.Check.Expression, check_opts: opts},
         path
       ) do
    expr = Keyword.get(opts, :expr)

    if expr do
      verify_expression_for_exists!(module, expr, path ++ [:check])
    end
  end

  defp verify_policy_check!(_module, _check, _path), do: :ok

  defp verify_expression_for_exists!(module, expr, path) do
    Ash.Expr.walk_template(expr, fn
      %Ash.Query.Exists{} = exists ->
        verify_exists_expression!(module, exists, path)
        exists

      other ->
        other
    end)
  end

  defp verify_exists_expression!(
         module,
         %Ash.Query.Exists{path: path, resource: resource, related?: related?},
         dsl_path
       ) do
    cond do
      not related? and is_atom(resource) ->
        verify_resource_exists!(module, resource, dsl_path)

      related? and is_list(path) ->
        verify_relationship_path!(module, path, dsl_path)

      true ->
        :ok
    end
  end

  defp verify_resource_exists!(module, resource, dsl_path) do
    if !resource_exists?(resource) do
      raise DslError,
        module: module,
        message: "Exists expression references undefined resource `#{inspect(resource)}`",
        path: dsl_path
    end
  end

  defp verify_relationship_path!(module, path, dsl_path) do
    case traverse_relationship_path(module, path) do
      {:error, invalid_relationship} ->
        message =
          if invalid_relationship in [:!=, :==, :>, :<, :>=, :<=] do
            suggested_fix =
              case path do
                [rel_name, op] when op in [:!=, :==, :>, :<, :>=, :<=] ->
                  "exists(#{rel_name}, <field> #{op} <value>)"

                _ ->
                  "exists(<relationship>, <filter_expression>)"
              end

            """
            Exists expression has invalid syntax. Found operator `#{invalid_relationship}` in relationship path.

            The correct syntax is: #{suggested_fix}
            """
          else
            "Exists expression references undefined relationship `#{invalid_relationship}` in path #{inspect(path)}"
          end

        raise DslError,
          module: module,
          message: message,
          path: dsl_path

      :ok ->
        :ok
    end
  end

  defp resource_exists?(resource) when is_atom(resource) do
    Code.ensure_loaded?(resource) and function_exported?(resource, :spark_dsl_config, 0)
  end

  defp traverse_relationship_path(_resource, []), do: :ok

  defp traverse_relationship_path(resource, [rel_name | rest]) do
    relationships = Ash.Resource.Info.relationships(resource)

    case Enum.find(relationships, &(&1.name == rel_name)) do
      nil ->
        {:error, rel_name}

      relationship ->
        traverse_relationship_path(relationship.destination, rest)
    end
  end
end
