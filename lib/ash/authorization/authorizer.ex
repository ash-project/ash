defmodule Ash.Authorization.Authorizer do
  @moduledoc """
  Determines if a set of authorization requests can be met or not.

  To read more about boolean satisfiability, see this page:
  https://en.wikipedia.org/wiki/Boolean_satisfiability_problem. At the end of
  the day, however, it is not necessary to understand exactly how Ash takes your
  authorization requirements and determines if a request is allowed. The
  important thing to understand is that Ash may or may not run any/all of your
  authorization rules as they may be deemed unnecessary. As such, authorization
  checks should have no side effects. Ideally, the checks built-in to ash should
  cover the bulk of your needs. If you need to write your own checks see #TODO:
  Link to a guide about writing checks here.
  """
  @type result :: :authorized | :forbidden

  alias Ash.Authorization.SatSolver

  # TODO: remove _context
  def authorize(user, _context, requests) do
    requests_by_relationship = requests_by_relationship(requests)

    authorization_steps = authorization_steps_with_relationship_path(requests_by_relationship)

    {facts, instructions} = strict_check_facts(user, requests)

    solve(authorization_steps, facts, instructions)
  end

  defp solve(authorization_steps, facts, instructions) do
    case SatSolver.solve(authorization_steps, facts) do
      {:error, :unsatisfiable} ->
        :forbidden

      {:ok, scenario} ->
        case get_all_scenarios(authorization_steps, scenario, facts, instructions) do
          {:ok, []} ->
            :forbidden

          {:ok, scenarios} ->
            irrelevant_clauses = irrelevant_clauses(scenarios)

            scenarios
            |> Enum.map(&Map.drop(&1, irrelevant_clauses))
            |> Enum.uniq()
            |> verify_scenarios(facts, instructions)
        end
    end
  end

  defp irrelevant_clauses(scenarios) do
    scenarios
    |> Enum.reduce([], fn scenario, acc ->
      scenario
      |> Enum.filter(fn {fact, result} ->
        Enum.any?(scenarios, fn potential_irrelevant_maker ->
          rest_of_scenario_matches =
            Map.delete(potential_irrelevant_maker, fact) == Map.delete(scenario, fact)

          fact_doesnt_match = {:ok, !result} == Map.fetch(potential_irrelevant_maker, fact)

          rest_of_scenario_matches && fact_doesnt_match
        end)
      end)
      |> Enum.map(&elem(&1, 0))
      |> Kernel.++(acc)
    end)
    |> Enum.uniq()
  end

  defp get_all_scenarios(
         authorization_steps,
         scenario,
         facts,
         instructions,
         negations \\ [],
         scenarios \\ nil
       ) do
    scenarios = scenarios || [scenario]

    case scenario_is_reality(scenario, facts) do
      :reality ->
        {:ok, scenarios}

      :not_reality ->
        raise "SAT SOLVER ERROR"

      :maybe ->
        negations_assuming_scenario_false = [scenario | negations]

        case SatSolver.solve(authorization_steps, facts, negations_assuming_scenario_false) do
          {:ok, scenario_after_negation} ->
            get_all_scenarios(
              authorization_steps,
              scenario_after_negation,
              facts,
              instructions,
              negations_assuming_scenario_false,
              scenarios
            )

          {:error, :unsatisfiable} ->
            {:ok, [scenario | scenarios]}
        end
    end
  end

  defp verify_scenarios(scenarios, facts, _instructions) do
    if any_scenarios_reality?(scenarios, facts) do
      :authorized
    else
      # TODO: Start gathering facts. If no facts remain to be gathered,
      # and no scenario is reality, then we are forbidden.
      # Use instructions from strict checks here.
      :forbidden
    end
  end

  defp any_scenarios_reality?(scenarios, facts) do
    Enum.any?(scenarios, fn scenario ->
      scenario_is_reality(scenario, facts) == :reality
    end)
  end

  defp scenario_is_reality(scenario, facts) do
    Enum.reduce_while(scenario, :reality, fn {fact, requirement}, status ->
      case Map.fetch(facts, fact) do
        {:ok, value} ->
          if value == requirement do
            if status == :reality do
              {:cont, :reality}
            else
              {:cont, status}
            end
          else
            {:halt, :not_reality}
          end

        :error ->
          {:halt, :maybe}
      end
    end)
  end

  defp strict_check_facts(user, requests) do
    Enum.reduce(requests, {%{true: true, false: false}, []}, fn request, {facts, instructions} ->
      {new_facts, new_instructions} = Ash.Authorization.Checker.strict_check(user, request, facts)

      {new_facts, instructions ++ new_instructions}
    end)
  end

  defp authorization_steps_with_relationship_path(requests_by_relationship) do
    Enum.flat_map(requests_by_relationship, fn {path, requests} ->
      Enum.map(requests, fn request ->
        Enum.map(request.authorization_steps, fn {step, fact} ->
          {step, {path, fact}}
        end)
      end)
    end)
  end

  defp requests_by_relationship(requests) do
    Enum.group_by(requests, &Map.get(&1, :relationship))
  end
end
