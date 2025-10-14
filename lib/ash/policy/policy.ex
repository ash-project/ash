# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Policy.Policy do
  @moduledoc "Represents a policy on an Ash.Resource"

  import Ash.SatSolver, only: [b: 1]

  alias Ash.Policy.Authorizer
  alias Ash.Policy.Check
  alias Ash.Policy.FieldPolicy
  alias Ash.SatSolver

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

  defguardp is_expression_operation(term)
            when (is_tuple(term) and (tuple_size(term) == 2 and elem(term, 0) == :not)) or
                   (tuple_size(term) == 3 and elem(term, 0) in [:and, :or])

  defguardp is_expression_check(term)
            when not is_expression_operation(term) and not is_boolean(term)

  # Temporarily making this private so that the function can be changed without
  # a major version bump in the SAT refactoring PR.
  # TODO: Make public with #2375 
  @doc false
  @spec expression(policies :: t() | FieldPolicy.t() | [t() | FieldPolicy.t()]) ::
          SatSolver.boolean_expr(Check.ref())
  def expression(policies) do
    policies
    |> List.wrap()
    |> Enum.map(fn policy ->
      # Simplification is important here to detect empty field policies
      cond_expr = policy |> condition_expression() |> simplify_policy_expression()
      pol_expr = policies_expression(policy)
      complete_expr = b(cond_expr and pol_expr)
      {policy, cond_expr, complete_expr}
    end)
    |> List.foldr({false, true}, fn
      {%{bypass?: true}, cond_expr, complete_expr}, {one_condition_matches, true} ->
        {
          b(cond_expr or one_condition_matches),
          # Bypass can't relay to the next bypass if there is none
          complete_expr
        }

      {%FieldPolicy{bypass?: true}, true, complete_expr},
      {one_condition_matches, all_policies_match} ->
        {
          # FieldPolicy Conditions are set to true by default and therefore
          # have to be ignore to not change the meaning of the
          # one_condition_matches condition
          one_condition_matches,
          b(complete_expr or all_policies_match)
        }

      {%{bypass?: true}, cond_expr, complete_expr}, {one_condition_matches, all_policies_match} ->
        {
          b(cond_expr or one_condition_matches),
          b(complete_expr or all_policies_match)
        }

      {%{}, cond_expr, complete_expr}, {one_condition_matches, all_policies_match} ->
        {
          b(cond_expr or one_condition_matches),
          b((complete_expr or not cond_expr) and all_policies_match)
        }
    end)
    |> then(&b(elem(&1, 0) and elem(&1, 1)))
    |> simplify_policy_expression()
  end

  @spec solve(authorizer :: Authorizer.t()) ::
          {:ok, boolean() | list(map), Authorizer.t()}
          | {:error, Authorizer.t(), Ash.Error.t()}
  def solve(authorizer) do
    {expression, authorizer} =
      build_requirements_expression(authorizer)

    case expression do
      expr when is_boolean(expr) ->
        {:ok, expr, authorizer}

      expression ->
        Ash.Policy.SatSolver.solve(expression, fn scenario, bindings ->
          scenario
          |> Ash.SatSolver.solutions_to_predicate_values(bindings)
          |> Map.drop([true, false])
        end)
        |> case do
          {:ok, scenarios} ->
            {:ok, Enum.uniq(scenarios), authorizer}

          {:error, error} ->
            {:error, authorizer, error}
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

  @spec build_requirements_expression(authorizer :: Authorizer.t()) ::
          {SatSolver.boolean_expr(Check.ref()), Authorizer.t()}
  defp build_requirements_expression(authorizer) do
    expression = expression(authorizer.policies)

    {expression, authorizer} =
      authorizer.facts
      |> Map.drop([true, false])
      |> Ash.Policy.SatSolver.facts_to_statement()
      |> case do
        nil -> expression
        facts_expression -> b(facts_expression and expression)
      end
      |> expand_constants(authorizer)

    expression = simplify_policy_expression(expression)

    authorizer = %{authorizer | solver_statement: expression}

    {expression, authorizer}
  end

  @spec fetch_or_strict_check_fact(
          Authorizer.t(),
          Check.t() | Check.ref()
        ) ::
          {:ok, SatSolver.boolean_expr(Check.ref()), Authorizer.t()}
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
          {:ok, SatSolver.boolean_expr(Check.ref())} | :error
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

  @spec condition_expression(policy :: t() | FieldPolicy.t()) ::
          SatSolver.boolean_expr(Check.ref())
  defp condition_expression(%{condition: condition}) do
    condition
    |> List.wrap()
    |> Enum.reduce(true, &b(&2 and &1))
  end

  @spec policies_expression(policy :: t() | FieldPolicy.t()) ::
          SatSolver.boolean_expr(Check.ref())
  defp policies_expression(%{policies: policies}) do
    policies
    |> List.wrap()
    |> List.foldr(false, fn
      %Check{type: :authorize_if} = clause, acc ->
        b({clause.check_module, clause.check_opts} or acc)

      %Check{type: :authorize_unless} = clause, acc ->
        b(not {clause.check_module, clause.check_opts} or acc)

      %Check{type: :forbid_if} = clause, acc ->
        b(not {clause.check_module, clause.check_opts} and acc)

      %Check{type: :forbid_unless} = clause, acc ->
        b({clause.check_module, clause.check_opts} and acc)
    end)
  end

  @spec expand_constants(
          expression :: SatSolver.boolean_expr(Check.ref()),
          authorizer :: Authorizer.t()
        ) :: {SatSolver.boolean_expr(Check.ref()), Authorizer.t()}
  defp expand_constants(expression, authorizer) do
    SatSolver.expand_expression(expression, authorizer, fn
      {Check.Static, opts}, authorizer ->
        {opts[:result], authorizer}

      expr, authorizer when is_expression_check(expr) ->
        case fetch_or_strict_check_fact(authorizer, expr) do
          {:ok, result, authorizer} ->
            {result, authorizer}

          {:error, authorizer} ->
            {expr, authorizer}
        end

      other, authorizer ->
        {other, authorizer}
    end)
  end

  @spec simplify_policy_expression(expression :: SatSolver.boolean_expr(Check.ref())) ::
          SatSolver.boolean_expr(Check.ref())
  defp simplify_policy_expression(expression) do
    expression
    |> SatSolver.walk_expression(fn
      {Check.Static, opts} -> opts[:result]
      other -> other
    end)
    |> SatSolver.simplify_expression()
  end

  @doc false
  @spec debug_expr(expr :: SatSolver.boolean_expr(Check.ref()), label :: String.t()) :: String.t()
  def debug_expr(expr, label \\ "Expr") do
    expr
    |> do_debug_expr()
    |> Macro.to_string()
    |> then(&"#{label}:\n\n#{&1}")
  end

  defp do_debug_expr(b(l and r)) do
    quote do
      unquote(do_debug_expr(l)) and unquote(do_debug_expr(r))
    end
  end

  defp do_debug_expr(b(l or r)) do
    quote do
      unquote(do_debug_expr(l)) or unquote(do_debug_expr(r))
    end
  end

  defp do_debug_expr(b(not v)) do
    quote do
      not unquote(do_debug_expr(v))
    end
  end

  defp do_debug_expr({check_module, check_opts}) do
    check_module.describe(check_opts)
  end

  defp do_debug_expr(v) do
    quote do
      unquote(Macro.escape(v))
    end
  end
end
