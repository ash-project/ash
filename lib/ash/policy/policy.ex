# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
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
    :error_message,
    :__spark_metadata__
  ]

  @type subject :: Ash.Query.t() | Ash.Changeset.t() | Ash.ActionInput.t()

  @type error_message_context :: %{
          resource: Ash.Resource.t(),
          action: Ash.Resource.Actions.action() | nil,
          actor: Ash.actor() | nil,
          domain: Ash.Domain.t() | nil,
          tenant: Ash.ToTenant.t() | nil
        }

  @type error_message ::
          String.t() | (subject(), error_message_context() -> String.t() | Exception.t())

  @type t :: %__MODULE__{
          condition: nil | Check.ref() | list(Check.ref()),
          policies: list(Check.t()),
          bypass?: boolean(),
          description: String.t() | nil,
          access_type: :strict | :filter | :runtime,
          error_message: error_message() | nil,
          __spark_metadata__: Spark.Dsl.Entity.spark_meta()
        }

  @spec expression(
          policies :: t() | FieldPolicy.t() | [t() | FieldPolicy.t()],
          check_context :: Check.context()
        ) :: Expression.t(Check.ref())
  def expression(policies, check_context) do
    policies = List.wrap(policies)

    check_context
    |> Map.get(:resource)
    |> Ash.Policy.Info.field_policy_expressions()
    |> Map.get(policies)
    |> case do
      nil -> policies |> raw_expression() |> simplify_policy_expression(check_context)
      expression -> expression
    end
    |> expand_invariants(check_context)
  end

  @spec raw_expression(policies :: t() | FieldPolicy.t() | [t() | FieldPolicy.t()]) ::
          Expression.t(Check.ref())
  defp raw_expression(policies) do
    policies
    |> List.wrap()
    |> Enum.map(fn policy ->
      cond_expr = condition_expression(policy)
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

  @doc """
  Evaluates a policy's check chain against already-computed facts and returns
  its decision.

  Walks checks in source order. The first decisive check fixes the decision:

    * `authorize_if X` with `X` true → `:authorized`
    * `authorize_unless X` with `X` false → `:authorized`
    * `forbid_if X` with `X` true → `:forbidden`
    * `forbid_unless X` with `X` false → `:forbidden`

  Anything else leaves the state at `:unknown` and the walk continues. If no
  check is decisive, the policy ends at `:unknown` — callers that need a
  binary "did this policy deny?" should treat `:unknown` as `:forbidden`,
  matching Ash's "if nothing authorized, the request is forbidden" rule.

  Reads from the supplied `facts` map only. Strict checks are **not** invoked,
  so this is safe to call from error-construction paths where rerunning a
  strict check would surface unrelated errors (e.g. missing calculation
  arguments). For pre-flight call sites that need lazy strict-check
  evaluation, use the private `policy_fails_statically?/2` in the policy
  authorizer.
  """
  @spec evaluate(t(), map()) :: :authorized | :forbidden | :unknown
  def evaluate(%__MODULE__{policies: checks}, facts) when is_map(facts) do
    Enum.reduce_while(List.wrap(checks), :unknown, fn check, _state ->
      case decide_check(check, facts) do
        :unknown -> {:cont, :unknown}
        decided -> {:halt, decided}
      end
    end)
  end

  defp decide_check(%Check{type: type, check_module: mod, check_opts: opts}, facts) do
    case fetch_fact(facts, {mod, opts}) do
      {:ok, true} when type == :authorize_if -> :authorized
      {:ok, true} when type == :forbid_if -> :forbidden
      {:ok, false} when type == :authorize_unless -> :authorized
      {:ok, false} when type == :forbid_unless -> :forbidden
      _ -> :unknown
    end
  end

  @doc """
  Returns the first non-bypass policy considered responsible for a forbidden
  outcome, along with its computed state.

  A policy is considered responsible if:

    * Its `condition` applies given the facts (no condition resolves to
      `{:ok, false}`),
    * It is not a bypass (bypasses don't deny on their own; an unsatisfied
      bypass just falls through to the next policy), and
    * Its `evaluate/2` decision is `:forbidden` (explicit deny) or `:unknown`
      (no check authorized).

  Within those, explicit `:forbidden` is preferred over `:unknown`. Returns
  `nil` when no non-bypass policy is responsible.
  """
  @spec responsible_for_forbidden([t() | FieldPolicy.t()], map()) ::
          {t(), :forbidden | :unknown} | nil
  def responsible_for_forbidden(policies, facts) when is_list(policies) and is_map(facts) do
    {forbidden, unknown} =
      Enum.reduce(policies, {nil, nil}, fn
        # Skip field policies — they don't carry `error_message` and the only
        # responsibility we surface is on regular policies.
        %FieldPolicy{}, acc ->
          acc

        %__MODULE__{bypass?: true}, acc ->
          acc

        %__MODULE__{} = policy, {forbidden_acc, unknown_acc} = acc ->
          if condition_applies?(policy, facts) do
            case evaluate(policy, facts) do
              :forbidden -> {forbidden_acc || policy, unknown_acc}
              :unknown -> {forbidden_acc, unknown_acc || policy}
              :authorized -> acc
            end
          else
            acc
          end
      end)

    cond do
      forbidden -> {forbidden, :forbidden}
      unknown -> {unknown, :unknown}
      true -> nil
    end
  end

  defp condition_applies?(%__MODULE__{condition: condition}, facts) do
    condition
    |> List.wrap()
    |> Enum.all?(fn check -> fetch_fact(facts, check) != {:ok, false} end)
  end

  @spec build_requirements_expression(
          authorizer :: Authorizer.t(),
          check_context :: Check.context()
        ) :: {Expression.t(Check.ref()), Authorizer.t()}
  defp build_requirements_expression(authorizer, check_context) do
    {expression, authorizer} = starting_expression(authorizer, check_context)

    {expression, authorizer} =
      expression
      |> simplify_checks(check_context)
      |> fold_constants(authorizer)

    expression =
      if is_boolean(expression) do
        expression
      else
        expression
        |> expand_invariants(check_context)
        |> Expression.simplify()
      end

    authorizer = %{authorizer | solver_statement: expression}

    {expression, authorizer}
  end

  @action_checks [
    Ash.Policy.Check.Action,
    Ash.Policy.Check.ActionType,
    Ash.Policy.Check.PrivateAction
  ]

  # Checks whose `simplify/2` we can safely call at compile time, i.e. the ones
  # shipped with Ash. User defined checks are left as-is (their `simplify/2` is
  # applied at runtime if the expression does not resolve statically).
  @statically_simplifiable [Ash.Policy.Check.Static, Ash.Policy.Check.ActorAbsent] ++
                             @action_checks

  @doc false
  # Builds the boolean expression for `policies` and simplifies it without any
  # knowledge of the request, except (optionally) the action. Used at compile
  # time by `Ash.Policy.Authorizer.Transformers.CachePolicyExpressions`.
  @spec static_expression(
          policies :: [t() | FieldPolicy.t()],
          resource :: Ash.Resource.t(),
          action :: Ash.Resource.Actions.action() | nil
        ) :: Expression.t(Check.ref())
  def static_expression(policies, resource, action) do
    check_context = %{resource: resource}

    policies
    |> raw_expression()
    |> Expression.postwalk(fn
      {check, opts} = ref when is_variable(ref) ->
        cond do
          action && check in @action_checks ->
            check.match?(nil, %{action: action, resource: resource}, opts)

          check in @statically_simplifiable ->
            Check.simplify(check, ref, check_context)

          true ->
            ref
        end

      other ->
        other
    end)
    |> Expression.simplify()
  end

  @spec starting_expression(Authorizer.t(), Check.context()) ::
          {Expression.t(Check.ref()), Authorizer.t()}
  defp starting_expression(
         %Authorizer{resource: resource, policies: policies, action: action} = authorizer,
         check_context
       ) do
    resource_expressions = Ash.Policy.Info.policy_expressions(resource)

    cond do
      # Domain policies are appended at runtime and change the expression, in
      # which case (as for anything else unexpected) we fall back to building it.
      resource_expressions && resource_expressions.policies == policies ->
        case action && Map.fetch(resource_expressions.by_action, action.name) do
          {:ok, expression} ->
            # The action checks were resolved at compile time, but their results
            # are still expected in `facts` (e.g. by the policy breakdown).
            {expression, add_action_facts(authorizer)}

          _ ->
            {resource_expressions.overall, authorizer}
        end

      expression = Ash.Policy.Info.field_policy_expressions(resource)[policies] ->
        {expression, authorizer}

      true ->
        {policies |> raw_expression() |> simplify_policy_expression(check_context), authorizer}
    end
  end

  @spec add_action_facts(Authorizer.t()) :: Authorizer.t()
  defp add_action_facts(%Authorizer{policies: policies} = authorizer) do
    policies
    |> Enum.flat_map(fn policy ->
      List.wrap(policy.condition) ++
        Enum.map(policy.policies, &{&1.check_module, &1.check_opts})
    end)
    |> Enum.filter(fn {check, _opts} -> check in @action_checks end)
    |> Enum.reduce(authorizer, fn ref, authorizer ->
      case fetch_or_strict_check_fact(authorizer, ref) do
        {:ok, _, authorizer} -> authorizer
        {:error, authorizer} -> authorizer
      end
    end)
  end

  # Evaluates every check reference that can be strict checked to a boolean
  # and folds the result through `and`/`or`/`not`, short-circuiting so that
  # checks in branches whose outcome is already determined are never run.
  #
  # This is purely constant folding: no boolean rewriting is performed, which
  # keeps it cheap. Anything left over is handed to the full pipeline.
  @spec fold_constants(
          expression :: Expression.t(Check.ref()),
          authorizer :: Authorizer.t()
        ) :: {Expression.t(Check.ref()), Authorizer.t()}
  defp fold_constants(expression, authorizer) when is_boolean(expression),
    do: {expression, authorizer}

  defp fold_constants(b(left and right), authorizer) do
    case fold_constants(left, authorizer) do
      {false, authorizer} ->
        {false, authorizer}

      {true, authorizer} ->
        fold_constants(right, authorizer)

      {left, authorizer} ->
        case fold_constants(right, authorizer) do
          {false, authorizer} -> {false, authorizer}
          {true, authorizer} -> {left, authorizer}
          {right, authorizer} -> {b(left and right), authorizer}
        end
    end
  end

  defp fold_constants(b(left or right), authorizer) do
    case fold_constants(left, authorizer) do
      {true, authorizer} ->
        {true, authorizer}

      {false, authorizer} ->
        fold_constants(right, authorizer)

      {left, authorizer} ->
        case fold_constants(right, authorizer) do
          {true, authorizer} -> {true, authorizer}
          {false, authorizer} -> {left, authorizer}
          {right, authorizer} -> {b(left or right), authorizer}
        end
    end
  end

  defp fold_constants(b(not expression), authorizer) do
    case fold_constants(expression, authorizer) do
      {value, authorizer} when is_boolean(value) -> {not value, authorizer}
      {expression, authorizer} -> {b(not expression), authorizer}
    end
  end

  defp fold_constants(expression, authorizer) when is_variable(expression) do
    case fetch_or_strict_check_fact(authorizer, expression) do
      {:ok, result, authorizer} -> {result, authorizer}
      {:error, authorizer} -> {expression, authorizer}
    end
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
        if Ash.Policy.Check.requires_original_data?(check_module, authorizer, opts) &&
             missing_original_data?(authorizer) do
          throw(
            {:error, authorizer,
             Ash.Error.Forbidden.InitialDataRequired.exception(
               source:
                 "check: #{Ash.Policy.Check.describe(check_module, opts)} requires initial data"
             )}
          )
        else
          case Ash.Policy.Check.strict_check(check_module, authorizer.actor, authorizer, opts) do
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

  @spec simplify_policy_expression(
          expression :: Expression.t(Check.ref()),
          context :: Check.context()
        ) :: Expression.t(Check.ref())
  defp simplify_policy_expression(expression, context) do
    expression
    |> simplify_checks(context)
    |> Expression.simplify()
  end

  # Applies every check's `simplify/2` (see `Ash.Policy.Check`), without any
  # boolean rewriting.
  @spec simplify_checks(
          expression :: Expression.t(Check.ref()),
          context :: Check.context()
        ) :: Expression.t(Check.ref())
  defp simplify_checks(expression, context) do
    Expression.postwalk(expression, fn
      {check, _opts} = expr when is_variable(expr) ->
        Code.ensure_loaded!(check)

        if function_exported?(check, :simplify, 2) do
          Ash.Policy.Check.simplify(check, expr, context)
        else
          expr
        end

      other ->
        other
    end)
  end

  @doc false
  @spec debug_expr(expr :: Expression.t(Check.ref()), label :: String.t()) :: String.t()
  def debug_expr(expr, label \\ "Expr") do
    expr
    |> Crux.Expression.to_string(fn
      {check_module, check_opts} -> Ash.Policy.Check.describe(check_module, check_opts)
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
            not function_exported?(check, :implies?, 3) ->
              acc

            Ash.Policy.Check.implies?(check, left, right, check_context) ->
              b(acc and implies(left, right))

            true ->
              acc
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
                      Ash.Policy.Check.PrivateAction,
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
          Ash.Policy.Check.conflicts?(check, left, right, check_context)
      end,
      implies?: fn {check, _opts} = left, right ->
        function_exported?(check, :implies?, 3) and
          Ash.Policy.Check.implies?(check, left, right, check_context)
      end
    ]
  end
end
