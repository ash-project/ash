defmodule Ash.Policy.Policy do
  @moduledoc false

  # For now we just write to `checks` and move them to `policies`
  # on build, when we support nested policies we can change that.
  defstruct [
    :condition,
    :policies,
    :bypass?,
    :checks,
    :description,
    :access_type
  ]

  @type t :: %__MODULE__{}

  def solve(authorizer) do
    authorizer.policies
    |> build_requirements_expression(authorizer.facts)
    |> Ash.Policy.SatSolver.solve()
  end

  defp build_requirements_expression(policies, facts) do
    at_least_one_policy_expression = at_least_one_policy_expression(policies, facts)

    policy_expression =
      {:and, at_least_one_policy_expression, compile_policy_expression(policies, facts)}

    facts_expression = Ash.Policy.SatSolver.facts_to_statement(Map.drop(facts, [true, false]))

    if facts_expression do
      {:and, facts_expression, policy_expression}
    else
      policy_expression
    end
  end

  def at_least_one_policy_expression(policies, facts) do
    policies
    |> Enum.map(&condition_expression(&1.condition, facts))
    |> Enum.filter(& &1)
    |> Enum.reduce(false, fn condition, acc ->
      {:or, condition, acc}
    end)
  end

  def fetch_fact(facts, %{check_module: mod, check_opts: opts}) do
    fetch_fact(facts, {mod, opts})
  end

  def fetch_fact(facts, {mod, opts}) do
    # TODO: this is slow, and we should figure out a better way to access facts indiscriminate of access type,
    # which my necessity must be stored with the fact (as facts create scenarios)
    # Eventually we may just want to track two separate maps of facts, one with access type and one without
    Enum.find_value(facts, fn
      {{fact_mod, fact_opts}, result} ->
        if mod == fact_mod &&
             Keyword.delete(fact_opts, :access_type) ==
               Keyword.delete(opts, :access_type) do
          {:ok, result}
        end

      _ ->
        nil
    end)
    |> case do
      nil ->
        :error

      value ->
        value
    end
  end

  defp condition_expression(condition, facts) do
    condition
    |> List.wrap()
    |> Enum.reduce(nil, fn
      condition, nil ->
        case fetch_fact(facts, condition) do
          {:ok, true} ->
            true

          {:ok, false} ->
            false

          _ ->
            condition
        end

      _condition, false ->
        false

      condition, expression ->
        case fetch_fact(facts, condition) do
          {:ok, true} ->
            expression

          {:ok, false} ->
            false

          _ ->
            {:and, condition, expression}
        end
    end)
  end

  defp compile_policy_expression(policies, facts)

  defp compile_policy_expression([], _facts) do
    false
  end

  defp compile_policy_expression(
         [%__MODULE__{condition: condition, policies: policies}],
         facts
       ) do
    compiled_policies = compile_policy_expression(policies, facts)
    condition_expression = condition_expression(condition, facts)

    case condition_expression do
      true ->
        compiled_policies

      false ->
        true

      nil ->
        compiled_policies

      condition_expression ->
        {:and, condition_expression, compiled_policies}
    end
  end

  defp compile_policy_expression(
         [
           %__MODULE__{condition: condition, policies: policies, bypass?: bypass?} | rest
         ],
         facts
       ) do
    condition_expression = condition_expression(condition, facts)

    case condition_expression do
      true ->
        if bypass? do
          {:or, compile_policy_expression(policies, facts),
           compile_policy_expression(rest, facts)}
        else
          {:and, compile_policy_expression(policies, facts),
           compile_policy_expression(rest, facts)}
        end

      false ->
        compile_policy_expression(rest, facts)

      nil ->
        if bypass? do
          {:or, compile_policy_expression(policies, facts),
           compile_policy_expression(rest, facts)}
        else
          {:and, compile_policy_expression(policies, facts),
           compile_policy_expression(rest, facts)}
        end

      condition_expression ->
        if bypass? do
          {:or, {:and, condition_expression, compile_policy_expression(policies, facts)},
           compile_policy_expression(rest, facts)}
        else
          {:or, {:and, condition_expression, compile_policy_expression(policies, facts)},
           {:and, {:not, condition_expression}, compile_policy_expression(rest, facts)}}
        end
    end
  end

  defp compile_policy_expression(
         [%{type: :authorize_if} = clause],
         facts
       ) do
    case fetch_fact(facts, clause) do
      {:ok, true} ->
        true

      {:ok, false} ->
        false

      :error ->
        {clause.check_module, clause.check_opts}
    end
  end

  defp compile_policy_expression(
         [%{type: :authorize_if} = clause | rest],
         facts
       ) do
    case fetch_fact(facts, clause) do
      {:ok, true} ->
        true

      {:ok, false} ->
        compile_policy_expression(rest, facts)

      :error ->
        {:or, {clause.check_module, clause.check_opts}, compile_policy_expression(rest, facts)}
    end
  end

  defp compile_policy_expression(
         [%{type: :authorize_unless} = clause],
         facts
       ) do
    case fetch_fact(facts, clause) do
      {:ok, true} ->
        false

      {:ok, false} ->
        true

      :error ->
        {clause.check_module, clause.check_opts}
    end
  end

  defp compile_policy_expression(
         [%{type: :authorize_unless} = clause | rest],
         facts
       ) do
    case fetch_fact(facts, clause) do
      {:ok, true} ->
        compile_policy_expression(rest, facts)

      {:ok, false} ->
        true

      :error ->
        {:or, {clause.check_module, clause.check_opts}, compile_policy_expression(rest, facts)}
    end
  end

  defp compile_policy_expression([%{type: :forbid_if}], _facts) do
    false
  end

  defp compile_policy_expression(
         [%{type: :forbid_if} = clause | rest],
         facts
       ) do
    case fetch_fact(facts, clause) do
      {:ok, true} ->
        false

      {:ok, false} ->
        compile_policy_expression(rest, facts)

      :error ->
        {:and, {:not, {clause.check_module, clause.check_opts}},
         compile_policy_expression(rest, facts)}
    end
  end

  defp compile_policy_expression([%{type: :forbid_unless}], _facts) do
    false
  end

  defp compile_policy_expression(
         [%{type: :forbid_unless} = clause | rest],
         facts
       ) do
    case fetch_fact(facts, clause) do
      {:ok, true} ->
        compile_policy_expression(rest, facts)

      {:ok, false} ->
        false

      :error ->
        {:or, {:not, {clause.check_module, clause.check_opts}},
         compile_policy_expression(rest, facts)}
    end
  end
end
