defmodule Ash.Policy.Checker do
  @moduledoc false

  alias Ash.Policy.{Check, FieldPolicy, Policy}

  def strict_check_all_facts(%{policies: policies} = authorizer) do
    Enum.reduce(policies, authorizer, fn policy, authorizer ->
      do_strict_check_all_facts(policy, authorizer)
    end)
  end

  defp do_strict_check_all_facts(%policy_struct{} = policy, authorizer)
       when policy_struct in [Policy, FieldPolicy] do
    policy.condition
    |> List.wrap()
    |> Enum.reduce(authorizer, fn {check_module, opts}, authorizer ->
      do_strict_check_all_facts(%Check{check_module: check_module, check_opts: opts}, authorizer)
    end)
    |> then(&strict_check_all_policies(policy.policies, &1))
  end

  defp do_strict_check_all_facts(%Ash.Policy.Check{} = check, authorizer) do
    check_module = check.check_module
    opts = check.check_opts

    case Ash.Policy.Policy.fetch_or_strict_check_fact(authorizer, {check_module, opts}) do
      {:ok, _, authorizer} ->
        authorizer

      {:error, authorizer} ->
        authorizer
    end
  end

  defp strict_check_all_policies(policies, authorizer) do
    Enum.reduce(policies, authorizer, fn policy, authorizer ->
      do_strict_check_all_facts(policy, authorizer)
    end)
  end

  def find_real_scenarios(scenarios, facts) do
    Enum.filter(scenarios, fn scenario ->
      scenario_is_reality(scenario, facts) == :reality
    end)
  end

  defp scenario_is_reality(scenario, facts) do
    scenario
    |> Map.drop([true, false])
    |> Enum.reduce_while(:reality, fn {fact, requirement}, status ->
      case Policy.fetch_fact(facts, fact) do
        {:ok, ^requirement} ->
          {:cont, status}

        {:ok, _} ->
          {:halt, :not_reality}

        :error ->
          {:cont, :maybe}
      end
    end)
  end

  def strict_check_scenarios(authorizer) do
    case Ash.Policy.Policy.solve(authorizer) do
      {:ok, value, authorizer} when is_boolean(value) ->
        {:ok, value, authorizer}

      {:ok, scenarios, authorizer} ->
        scenarios
        |> remove_scenarios_with_impossible_facts(authorizer)
        |> Ash.Policy.SatSolver.simplify_clauses()
        |> case do
          [] -> {:ok, false, authorizer}
          scenarios -> {:ok, scenarios, authorizer}
        end

      {:error, authorizer, error} ->
        {:error, authorizer, error}
    end
  end

  defp remove_scenarios_with_impossible_facts(scenarios, authorizer) do
    Enum.reject(scenarios, fn scenario ->
      Enum.any?(scenario, fn {{mod, opts}, required_value} ->
        case Policy.fetch_fact(authorizer.facts, {mod, opts}) do
          {:ok, :unknown} ->
            opts[:access_type] == :strict

          {:ok, value} ->
            value != required_value

          :error ->
            opts[:access_type] == :strict
        end
      end)
    end)
  end
end
