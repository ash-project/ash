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
  cover the bulk of your needs.

  If you need to write your own checks see #TODO: Link to a guide about writing checks here.
  """
  @type result :: :authorized | :forbidden

  alias Ash.Authorization.SatSolver

  # TODO: remove _context
  def authorize(user, _context, requests) do
    requests_by_relationship = requests_by_relationship(requests)

    authorization_steps = authorization_steps_with_relationship_path(requests_by_relationship)

    if Enum.any?(authorization_steps, &Enum.empty?/1) do
      {:error, Ash.Error.Forbidden.exception(no_steps_configured?: true)}
    else
      facts = strict_check_facts(user, requests)

      solve(authorization_steps, facts, facts, %{user: user})
    end
  end

  defp solve(authorization_steps, facts, strict_check_facts, state) do
    case sat_solver(authorization_steps, facts, [], state) do
      {:error, :unsatisfiable} ->
        {:error,
         Ash.Error.Forbidden.exception(
           authorization_steps: authorization_steps,
           facts: facts,
           strict_check_facts: strict_check_facts,
           state: state
         )}

      {:ok, scenario} ->
        authorization_steps
        |> get_all_scenarios(scenario, facts, state)
        |> Enum.uniq()
        |> remove_irrelevant_clauses()
        |> verify_scenarios(authorization_steps, facts, strict_check_facts, state)
    end
  end

  defp remove_irrelevant_clauses(scenarios) do
    new_scenarios =
      scenarios
      |> Enum.uniq()
      |> Enum.map(fn scenario ->
        unnecessary_fact =
          Enum.find_value(scenario, fn
            {_fact, :unknowable} ->
              false

            {fact, value_in_this_scenario} ->
              matching =
                Enum.find(scenarios, fn potential_irrelevant_maker ->
                  potential_irrelevant_maker != scenario &&
                    Map.delete(scenario, fact) == Map.delete(potential_irrelevant_maker, fact)
                end)

              case matching do
                %{^fact => value} when is_boolean(value) and value != value_in_this_scenario ->
                  fact

                _ ->
                  false
              end
          end)

        Map.delete(scenario, unnecessary_fact)
      end)
      |> Enum.uniq()

    if new_scenarios == scenarios do
      new_scenarios
    else
      remove_irrelevant_clauses(new_scenarios)
    end
  end

  defp get_all_scenarios(
         authorization_steps,
         scenario,
         facts,
         state,
         negations \\ [],
         scenarios \\ []
       ) do
    scenario = Map.drop(scenario, [true, false])
    scenarios = [scenario | scenarios]

    case scenario_is_reality(scenario, facts) do
      :reality ->
        scenarios

      :not_reality ->
        raise "SAT SOLVER ERROR"

      :maybe ->
        negations_assuming_scenario_false = [scenario | negations]

        case sat_solver(
               authorization_steps,
               facts,
               negations_assuming_scenario_false,
               state
             ) do
          {:ok, scenario_after_negation} ->
            get_all_scenarios(
              authorization_steps,
              scenario_after_negation,
              facts,
              state,
              negations_assuming_scenario_false,
              scenarios
            )

          {:error, :unsatisfiable} ->
            scenarios
        end
    end
  end

  defp sat_solver(authorization_steps, facts, negations, state) do
    case state do
      %{data: [%resource{} | _] = data} ->
        # TODO: Needs primary key
        pkey = Ash.primary_key(resource)

        ids = Enum.map(data, &Map.take(&1, pkey))
        SatSolver.solve(authorization_steps, facts, negations, ids)

      _ ->
        SatSolver.solve(authorization_steps, facts, negations, nil)
    end
  end

  defp verify_scenarios(
         scenarios,
         authorization_steps,
         facts,
         strict_check_facts,
         state
       ) do
    if any_scenarios_reality?(scenarios, facts) do
      if Map.has_key?(state, :data) do
        :ok
      else
        {:ok, fn _ -> :ok end}
      end
    else
      if Map.has_key?(state, :data) do
        case fetch_facts(scenarios, facts, state) do
          :all_scenarios_known ->
            {:error,
             Ash.Error.Forbidden.exception(
               scenarios: scenarios,
               authorization_steps: authorization_steps,
               facts: facts,
               strict_check_facts: strict_check_facts,
               state: state
             )}

          {:error, error} ->
            {:error, error}

          {:ok, new_facts, state} ->
            solve(authorization_steps, new_facts, strict_check_facts, state)
        end
      else
        callback = fn data ->
          data = List.wrap(data)

          if data == [] do
            {:ok, fn _ -> :ok end}
          else
            verify_scenarios(
              scenarios,
              authorization_steps,
              facts,
              strict_check_facts,
              Map.put(state, :data, List.wrap(data))
            )
          end
        end

        {:ok, callback}
      end
    end
  end

  defp fetch_facts(scenarios, facts, state) do
    Ash.Authorization.Checker.run_checks(scenarios, facts, state)
  end

  defp any_scenarios_reality?(scenarios, facts) do
    Enum.any?(scenarios, fn scenario ->
      scenario_is_reality(scenario, facts) == :reality
    end)
  end

  defp scenario_is_reality(scenario, facts) do
    scenario
    |> Map.drop([true, false])
    |> Enum.reduce_while(:reality, fn {fact, requirement}, status ->
      case Map.fetch(facts, fact) do
        {:ok, value} ->
          cond do
            value == requirement ->
              {:cont, status}

            value == :unknowable ->
              {:halt, :not_reality}

            true ->
              {:halt, :not_reality}
          end

        :error ->
          {:cont, :maybe}
      end
    end)
  end

  defp strict_check_facts(user, requests) do
    Enum.reduce(requests, %{true: true, false: false}, fn request, facts ->
      Ash.Authorization.Checker.strict_check(user, request, facts)
    end)
  end

  defp authorization_steps_with_relationship_path(requests_by_relationship) do
    Enum.flat_map(requests_by_relationship, fn {path, requests} ->
      Enum.map(requests, fn request ->
        Enum.map(request.authorization_steps, fn {step, fact} ->
          {step, Ash.Authorization.Clause.new(path, request.resource, fact)}
        end)
      end)
    end)
  end

  defp requests_by_relationship(requests) do
    Enum.group_by(requests, &Map.get(&1, :relationship))
  end
end
