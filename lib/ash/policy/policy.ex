# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Policy.Policy do
  @moduledoc "Represents a policy on an Ash.Resource"

  import Crux.Expression, only: [b: 1, is_variable: 1]

  alias Ash.Policy.Authorizer
  alias Ash.Policy.Check
  alias Ash.Policy.FieldPolicy
  alias Crux
  alias Crux.Expression
  alias Crux.Formula

  # For now we just write to `checks` and move them to `policies`
  # on build, when we support nested policies we can change that.
  defstruct [
    :condition,
    :policies,
    :bypass?,
    :description,
    :access_type,
    :__spark_metadata__
  ]

  @type t :: %__MODULE__{
          condition: nil | Check.ref() | list(Check.ref()),
          policies: list(Check.t()),
          bypass?: boolean(),
          description: String.t() | nil,
          access_type: :strict | :filter | :runtime,
          __spark_metadata__: Spark.Dsl.Entity.spark_meta()
        }

  @spec expression(
          policies :: t() | FieldPolicy.t() | [t() | FieldPolicy.t()],
          check_context :: Check.context()
        ) :: Expression.t(Check.ref())
  def expression(policies, check_context) do
    policies
    |> List.wrap()
    |> Enum.map(fn policy ->
      # Simplification is important here to detect empty field policies
      cond_expr = policy |> condition_expression() |> simplify_policy_expression(check_context)
      pol_expr = policies_expression(policy)
      complete_expr = b(cond_expr and pol_expr)
      {policy, cond_expr, complete_expr}
    end)
    |> List.foldr({false, true}, fn
      {%{bypass?: true}, _cond_expr, complete_expr},
      {one_condition_matches, all_policies_match} ->
        {
          # Bypass should only contribute to "at least one policy applies" if it actually authorizes.
          # Use complete_expr (condition AND policies) not just condition.
          b(complete_expr or one_condition_matches),
          b(complete_expr or all_policies_match)
        }

      {%{}, cond_expr, complete_expr}, {one_condition_matches, all_policies_match} ->
        {
          b(cond_expr or one_condition_matches),
          b(implied_by(complete_expr, cond_expr) and all_policies_match)
        }
    end)
    |> then(&b(elem(&1, 0) and elem(&1, 1)))
    |> simplify_policy_expression(check_context)
    |> expand_invariants(check_context)
  end

  @spec solve(authorizer :: Authorizer.t()) ::
          {:ok, boolean() | list(map), Authorizer.t()}
          | {:error, Authorizer.t(), Ash.Error.t()}
  def solve(authorizer) do
    check_context = check_context(authorizer)

    {expression, authorizer} =
      build_requirements_expression(authorizer, check_context)

    case expression do
      expr when is_boolean(expr) ->
        {:ok, expr, authorizer}

      expression ->
        scenario_options = scenario_options(check_context)

        expression
        |> Formula.from_expression()
        |> Crux.satisfying_scenarios(scenario_options)
        |> case do
          [] ->
            {:error, authorizer, :unsatisfiable}

          scenarios ->
            mapped_scenarios = Enum.map(scenarios, &Map.drop(&1, [true, false]))
            {:ok, Enum.uniq(mapped_scenarios), authorizer}
        end
    end
  catch
    {:error, authorizer, error} ->
      {:error, authorizer, error}
  end

  @doc false
  @spec transform(policy :: t()) :: {:ok, t()} | {:error, String.t()}
  def transform(policy) do
    cond do
      policy.policies |> List.wrap() |> Enum.empty?() ->
        {:error, "Policies must have at least one check."}

      policy.bypass? &&
          Enum.all?(List.wrap(policy.policies), &(&1.type in [:forbid_if, :forbid_unless])) ->
        {:error,
         """
         Bypass policies that can only ever forbid have no effect.

         When a bypass is authorized, it skips all remaining policies (including other bypasses)
         and authorizes the request. If it fails, it is ignored and the remaining policies are checked.

         This policy only contains `forbid_if` or `forbid_unless` check types therefore, it can
         never have an effect.
         """}

      policy.condition |> List.wrap() |> Enum.empty?() ->
        {:ok, %{policy | condition: [{Check.Static, result: true}]}}

      true ->
        {:ok, policy}
    end
  end

  @spec build_requirements_expression(
          authorizer :: Authorizer.t(),
          check_context :: Check.context()
        ) :: {Expression.t(Check.ref()), Authorizer.t()}
  defp build_requirements_expression(authorizer, check_context) do
    {expression, authorizer} =
      authorizer.policies
      |> expression(check_context)
      |> simplify_policy_expression(check_context)
      |> expand_constants(authorizer, check_context)

    authorizer = %{authorizer | solver_statement: expression}

    {expression, authorizer}
  end

  @spec fetch_or_strict_check_fact(
          Authorizer.t(),
          Check.t() | Check.ref()
        ) ::
          {:ok, Expression.t(Check.ref()), Authorizer.t()}
          | {:error, Authorizer.t()}
  def fetch_or_strict_check_fact(authorizer, check)

  def fetch_or_strict_check_fact(authorizer, %Check{check_module: mod, check_opts: opts}),
    do: fetch_or_strict_check_fact(authorizer, {mod, opts})

  def fetch_or_strict_check_fact(authorizer, check) when is_atom(check),
    do: fetch_or_strict_check_fact(authorizer, {check, []})

  def fetch_or_strict_check_fact(authorizer, {Check.Static, opts}),
    do: {:ok, opts[:result], authorizer}

  def fetch_or_strict_check_fact(authorizer, {check_module, opts}) do
    authorizer.facts
    |> Enum.find_value(fn
      {{fact_mod, fact_opts}, result} when result != :unknown ->
        if check_module == fact_mod &&
             Keyword.drop(fact_opts, [:access_type, :ash_field_policy?]) ==
               Keyword.drop(opts, [:access_type, :ash_field_policy?]) do
          {:ok, result}
        end

      _ ->
        nil
    end)
    |> case do
      nil ->
        if check_module.requires_original_data?(authorizer, opts) &&
             missing_original_data?(authorizer) do
          throw(
            {:error, authorizer,
             Ash.Error.Forbidden.InitialDataRequired.exception(
               source: "check: #{check_module.describe(opts)} requires initial data"
             )}
          )
        else
          case check_module.strict_check(authorizer.actor, authorizer, opts) do
            {:ok, value} when is_boolean(value) or value == :unknown ->
              authorizer = %{
                authorizer
                | facts: Map.put(authorizer.facts, {check_module, opts}, value)
              }

              if value == :unknown do
                {:error, authorizer}
              else
                {:ok, value, authorizer}
              end

            {:error, error} ->
              throw({:error, authorizer, Ash.Error.to_ash_error(error)})
          end
        end

      {:ok, :unknown} ->
        {:error, authorizer}

      {:ok, value} ->
        {:ok, value, authorizer}
    end
  end

  @spec missing_original_data?(authorizer :: Authorizer.t()) :: boolean()
  defp missing_original_data?(%Authorizer{
         changeset: %Ash.Changeset{data: %Ash.Changeset.OriginalDataNotAvailable{}}
       }) do
    true
  end

  defp missing_original_data?(_), do: false

  @spec fetch_fact(facts :: map, check :: Check.t() | Check.ref()) ::
          {:ok, Expression.t(Check.ref())} | :error
  def fetch_fact(facts, check)

  def fetch_fact(facts, %{check_module: mod, check_opts: opts}),
    do: fetch_fact(facts, {mod, opts})

  def fetch_fact(_facts, {Check.Static, opts}), do: {:ok, opts[:result]}

  def fetch_fact(facts, {mod, opts}) do
    Enum.find_value(facts, fn
      {{fact_mod, fact_opts}, result} ->
        if mod == fact_mod &&
             Keyword.drop(fact_opts, [:access_type, :ash_field_policy?]) ==
               Keyword.drop(opts, [:access_type, :ash_field_policy?]) do
          {:ok, result}
        end

      _ ->
        nil
    end)
    |> case do
      nil ->
        :error

      :unknown ->
        :error

      value ->
        value
    end
  end

  @spec condition_expression(policy :: t() | FieldPolicy.t()) :: Expression.t(Check.ref())
  defp condition_expression(%{condition: condition}) do
    condition
    |> List.wrap()
    |> Enum.reduce(true, &b(&2 and &1))
  end

  @spec policies_expression(policy :: t() | FieldPolicy.t()) :: Expression.t(Check.ref())
  defp policies_expression(%{policies: policies}) do
    policies
    |> List.wrap()
    |> List.foldr(false, fn
      %Check{type: :authorize_if} = clause, acc ->
        b({clause.check_module, clause.check_opts} or acc)

      %Check{type: :authorize_unless} = clause, acc ->
        b(implies({clause.check_module, clause.check_opts}, acc))

      %Check{type: :forbid_if} = clause, acc ->
        b(not {clause.check_module, clause.check_opts} and acc)

      %Check{type: :forbid_unless} = clause, acc ->
        b({clause.check_module, clause.check_opts} and acc)
    end)
  end

  @spec expand_constants(
          expression :: Expression.t(Check.ref()),
          authorizer :: Authorizer.t(),
          check_context :: Check.context()
        ) :: {Expression.t(Check.ref()), Authorizer.t()}
  defp expand_constants(expression, authorizer, check_context) do
    {expression, authorizer} =
      Expression.expand(expression, authorizer, fn
        expr, authorizer when is_variable(expr) ->
          case fetch_or_strict_check_fact(authorizer, expr) do
            {:ok, result, authorizer} ->
              {result, authorizer}

            {:error, authorizer} ->
              {expr, authorizer}
          end

        other, authorizer ->
          {other, authorizer}
      end)

    {simplify_policy_expression(expression, check_context), authorizer}
  end

  @spec simplify_policy_expression(
          expression :: Expression.t(Check.ref()),
          context :: Check.context()
        ) :: Expression.t(Check.ref())
  defp simplify_policy_expression(expression, context) do
    expression
    |> Expression.postwalk(fn
      {check, _opts} = expr when is_variable(expr) ->
        Code.ensure_loaded!(check)

        if function_exported?(check, :simplify, 2) do
          check.simplify(expr, context)
        else
          expr
        end

      other ->
        other
    end)
    |> Expression.simplify()
  end

  @doc false
  @spec debug_expr(expr :: Expression.t(Check.ref()), label :: String.t()) :: String.t()
  def debug_expr(expr, label \\ "Expr") do
    expr
    |> Crux.Expression.to_string(fn
      {check_module, check_opts} -> check_module.describe(check_opts)
      v -> Macro.escape(v)
    end)
    |> then(&"#{label}:\n\n#{&1}")
  end

  @spec check_context(authorizer :: Authorizer.t()) :: Check.context()
  defp check_context(%Authorizer{resource: resource}) do
    %{resource: resource}
  end

  @spec expand_invariants(
          expression :: Expression.t(Check.ref()),
          check_context :: Check.context()
        ) :: Expression.t(Check.ref())
  defp expand_invariants(expression, check_context) do
    {_, variables} =
      Expression.postwalk(expression, [], fn
        check, acc when is_variable(check) -> {check, [check | acc]}
        other, acc -> {other, acc}
      end)

    unique_variables = Enum.uniq(variables)

    for {check, _} <- unique_variables do
      Code.ensure_loaded!(check)
    end

    expression =
      for {check, _} = left <- unique_variables,
          right <- unique_variables,
          left != right,
          reduce: expression do
        acc ->
          cond do
            not function_exported?(check, :implies?, 3) -> acc
            check.implies?(left, right, check_context) -> b(acc and implies(left, right))
            true -> acc
          end
      end

    for {check, _} = left <- unique_variables,
        right <- unique_variables,
        left != right,
        reduce: expression do
      acc ->
        cond do
          not function_exported?(check, :conflict?, 3) -> acc
          check.conflict?(left, right, check_context) -> b(acc and nand(left, right))
          true -> acc
        end
    end
  end

  @check_priorities [
                      Ash.Policy.Check.Static,
                      Ash.Policy.Check.Action,
                      Ash.Policy.Check.ActionType,
                      Ash.Policy.Check.ActorAbsent,
                      Ash.Policy.Check.ActorPresent
                    ]
                    |> Enum.with_index()
                    |> Map.new()

  @doc """
  Default Options for Crux scenarios
  """
  @spec scenario_options(check_context :: Check.context()) :: Crux.opts(Check.ref())
  def scenario_options(check_context) do
    [
      sorter: fn left, right ->
        left_priority = Map.get(@check_priorities, elem(left, 0), 1_000)
        right_priority = Map.get(@check_priorities, elem(right, 0), 1_000)

        if left_priority != right_priority do
          left_priority <= right_priority
        else
          left <= right
        end
      end,
      conflicts?: fn {check, _otps} = left, right ->
        function_exported?(check, :conflicts?, 3) and
          check.conflicts?(left, right, check_context)
      end,
      implies?: fn {check, _opts} = left, right ->
        function_exported?(check, :implies?, 3) and
          check.implies?(left, right, check_context)
      end
    ]
  end
end
