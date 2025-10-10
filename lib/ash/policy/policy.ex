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

  @static_checks [
    {Check.Static, [result: true]},
    {Check.Static, [result: false]},
    true,
    false
  ]

  @spec expression(policies :: t() | FieldPolicy.t() | [t() | FieldPolicy.t()]) ::
          SatSolver.boolean_expr(Check.ref())
  def expression(policies) do
    policies = List.wrap(policies)

    at_least_one_policy_expression =
      at_least_one_policy_expression(policies)

    policy_expression =
      compile_policy_expression(policies)

    simplify_policy_expression({:and, at_least_one_policy_expression, policy_expression})
  end

  @spec solve(authorizer :: Authorizer.t()) ::
          {:ok, boolean() | list(map), Authorizer.t()}
          | {:error, Authorizer.t(), Ash.Error.t()}
  def solve(authorizer) do
    authorizer = strict_check_all_conditions(authorizer)

    policies = policies_that_may_apply(authorizer)

    {expression, authorizer} =
      build_requirements_expression(policies, authorizer)

    case expression do
      expr when is_boolean(expr) ->
        {:ok, expr, authorizer}

      expression ->
        Ash.Policy.SatSolver.solve(expression, fn scenario, bindings ->
          scenario
          |> Ash.SatSolver.solutions_to_predicate_values(bindings)
          |> Map.drop(@static_checks)
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

  defp strict_check_all_conditions(authorizer) do
    Enum.reduce(authorizer.policies || [], authorizer, fn policy, authorizer ->
      Enum.reduce_while(policy.condition || [], authorizer, fn condition, authorizer ->
        case fetch_or_strict_check_fact(authorizer, condition) do
          {:ok, true, authorizer} -> {:cont, authorizer}
          {:ok, _, authorizer} -> {:halt, authorizer}
          {:error, authorizer} -> {:halt, authorizer}
        end
      end)
    end)
  end

  defp policies_that_may_apply(authorizer) do
    Enum.filter(authorizer.policies || [], fn policy ->
      Enum.all?(policy.condition || [], fn condition ->
        case fetch_fact(authorizer.facts, condition) do
          {:ok, true} -> true
          {:ok, false} -> false
          {:ok, :unknown} -> true
          :error -> true
        end
      end)
    end)
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
          policies :: t() | FieldPolicy.t() | [t() | FieldPolicy.t()],
          authorizer :: Authorizer.t()
        ) ::
          {SatSolver.boolean_expr(Check.ref()), Authorizer.t()}
  defp build_requirements_expression(policies, authorizer) do
    policy_and_condition_expression = expression(policies)

    authorizer = %{authorizer | solver_statement: policy_and_condition_expression}

    facts_expression =
      authorizer.facts
      |> Map.drop([true, false])
      |> Ash.Policy.SatSolver.facts_to_statement()

    if is_nil(facts_expression) do
      handle_constants(policy_and_condition_expression, authorizer)
    else
      handle_constants({:and, facts_expression, policy_and_condition_expression}, authorizer)
    end
  end

  # at least one policy must apply
  # or one bypass must authorize
  def at_least_one_policy_expression(policies) do
    policies
    |> List.wrap()
    |> Enum.reduce(false, fn policy, condition_exprs ->
      if policy.bypass? do
        if condition_exprs == false do
          compile_policy_expression([policy])
        else
          {:or, compile_policy_expression([policy]), condition_exprs}
        end
      else
        if condition_exprs == false do
          condition_expression(policy)
        else
          {:or, condition_expression(policy), condition_exprs}
        end
      end
    end)
  end

  @spec fetch_or_strict_check_fact(
          Authorizer.t(),
          Check.t() | Check.ref()
        ) ::
          {:ok, SatSolver.boolean_expr(Check.ref()), Authorizer.t()}
          | {:error, Authorizer.t()}
  def fetch_or_strict_check_fact(authorizer, check)

  def fetch_or_strict_check_fact(authorizer, %{check_module: mod, check_opts: opts}) do
    fetch_or_strict_check_fact(authorizer, {mod, opts})
  end

  def fetch_or_strict_check_fact(authorizer, {Check.Static, opts}) do
    {:ok, opts[:result], authorizer}
  end

  def fetch_or_strict_check_fact(authorizer, {check_module, opts}) do
    Enum.find_value(authorizer.facts, fn
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

  def fetch_fact(facts, %{check_module: mod, check_opts: opts}) do
    fetch_fact(facts, {mod, opts})
  end

  def fetch_fact(_facts, {Check.Static, opts}) do
    {:ok, opts[:result]}
  end

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

  defp compile_policy_expression([]) do
    false
  end

  defp compile_policy_expression(nil) do
    false
  end

  defp compile_policy_expression([
         %struct{bypass?: bypass?} = policy
       ])
       when struct in [__MODULE__, Ash.Policy.FieldPolicy] do
    condition_expression = condition_expression(policy)
    policy_expression = policies_expression(policy)

    if bypass? do
      {:and, condition_expression, policy_expression}
    else
      {:or, {:and, condition_expression, policy_expression}, {:not, condition_expression}}
    end
  end

  defp compile_policy_expression([
         %{bypass?: bypass?} = policy | rest
       ]) do
    condition_expression = condition_expression(policy)
    policy_expression = policies_expression(policy)

    if bypass? do
      condition_and_policy_expression = {:and, condition_expression, policy_expression}

      rest = compile_policy_expression(rest)

      {:or, condition_and_policy_expression, rest}
    else
      rest_expr = compile_policy_expression(rest)

      {:and, {:or, {:not, condition_expression}, {:and, condition_expression, policy_expression}},
       rest_expr}
    end
  end

  defp clean_constant_checks({combinator, left, right}) when combinator in [:and, :or] do
    left = clean_constant_checks(left)
    right = clean_constant_checks(right)

    case {left, right} do
      {{Check.Static, left_opts}, {Check.Static, right_opts}} ->
        if left_opts[:result] && right_opts[:result] do
          {Check.Static, Keyword.put(left_opts, :result, true)}
        else
          {combinator, left, right}
        end

      {{Check.Static, left_opts}, right} ->
        if left_opts[:result] do
          right
        else
          {combinator, left, right}
        end

      {left, {Check.Static, right_opts}} ->
        if right_opts[:result] do
          left
        else
          {combinator, left, right}
        end

      {left, right} ->
        {combinator, left, right}
    end
  end

  defp clean_constant_checks({:not, expr}) do
    case clean_constant_checks(expr) do
      {Check.Static, opts} ->
        {Check.Static, Keyword.put(opts, :result, !opts[:result])}

      other ->
        {:not, other}
    end
  end

  defp clean_constant_checks(other), do: other

  defp handle_constants({:and, l, r}, authorizer) do
    case handle_constants(l, authorizer) do
      {false, authorizer} ->
        {false, authorizer}

      {l, authorizer} ->
        case {l, handle_constants(r, authorizer)} do
          {_, {false, authorizer}} -> {false, authorizer}
          {false, {_, authorizer}} -> {false, authorizer}
          {true, {true, authorizer}} -> {true, authorizer}
          {true, {r, authorizer}} -> {r, authorizer}
          {l, {true, authorizer}} -> {l, authorizer}
          {l, {r, authorizer}} -> {{:and, l, r}, authorizer}
        end
    end
  end

  defp handle_constants({:or, l, r}, authorizer) do
    case handle_constants(l, authorizer) do
      {true, authorizer} ->
        {true, authorizer}

      {l, authorizer} ->
        case {l, handle_constants(r, authorizer)} do
          {_, {true, authorizer}} -> {true, authorizer}
          {true, {_, authorizer}} -> {true, authorizer}
          {false, {false, authorizer}} -> {false, authorizer}
          {false, {r, authorizer}} -> {r, authorizer}
          {l, {false, authorizer}} -> {l, authorizer}
          {l, {r, authorizer}} -> {{:or, l, r}, authorizer}
        end
    end
  end

  defp handle_constants({:not, l}, authorizer) do
    case handle_constants(l, authorizer) do
      {true, authorizer} -> {false, authorizer}
      {false, authorizer} -> {true, authorizer}
      {l, authorizer} -> {{:not, l}, authorizer}
    end
  end

  defp handle_constants({mod, opts}, authorizer) do
    case fetch_or_strict_check_fact(authorizer, {mod, opts}) do
      {:ok, result, authorizer} ->
        {result, authorizer}

      {:error, authorizer} ->
        {{mod, opts}, authorizer}
    end
  end

  defp handle_constants(other, authorizer), do: {other, authorizer}

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
    |> clean_constant_checks()
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
