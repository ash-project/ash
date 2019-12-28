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
  alias Ash.Authorization.Request

  def authorize(user, requests, opts \\ []) do
    strict_access? = Keyword.get(opts, :strict_access?, true)

    if Enum.any?(requests, fn request -> Enum.empty?(request.authorization_steps) end) do
      {:error, Ash.Error.Forbidden.exception(no_steps_configured?: true)}
    else
      facts = strict_check_facts(user, requests, strict_access?)

      solve(requests, user, facts, facts, %{user: user}, strict_access?)
    end
  end

  defp solve(requests, user, facts, strict_check_facts, state, strict_access?) do
    case sat_solver(requests, facts, [], state) do
      {:error, :unsatisfiable} ->
        {:error,
         Ash.Error.Forbidden.exception(
           requests: requests,
           facts: facts,
           strict_check_facts: strict_check_facts,
           strict_access?: strict_access?,
           state: state
         )}

      {:ok, scenario} ->
        requests
        |> get_all_scenarios(scenario, facts, state)
        |> Enum.uniq()
        |> remove_irrelevant_clauses()
        |> verify_scenarios(user, requests, facts, strict_check_facts, state, strict_access?)
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
         requests,
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
               requests,
               facts,
               negations_assuming_scenario_false,
               state
             ) do
          {:ok, scenario_after_negation} ->
            get_all_scenarios(
              requests,
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

  defp sat_solver(requests, facts, negations, state) do
    case state do
      %{data: [%resource{} | _] = data} ->
        # TODO: Needs primary key
        pkey = Ash.primary_key(resource)

        ids = Enum.map(data, &Map.take(&1, pkey))
        SatSolver.solve(requests, facts, negations, ids)

      _ ->
        SatSolver.solve(requests, facts, negations, nil)
    end
  end

  defp verify_scenarios(
         scenarios,
         user,
         requests,
         facts,
         strict_check_facts,
         state,
         strict_access?
       ) do
    if any_scenarios_reality?(scenarios, facts) do
      fetch_must_fetch(requests, state)
    else
      case Ash.Authorization.Checker.run_checks(
             scenarios,
             user,
             requests,
             facts,
             state,
             strict_access?
           ) do
        :all_scenarios_known ->
          error =
            Ash.Error.Forbidden.exception(
              scenarios: scenarios,
              requests: requests,
              facts: facts,
              strict_check_facts: strict_check_facts,
              state: state,
              strict_access?: strict_access?
            )

          {:error, error}

        {:error, error} ->
          {:error, error}

        {:ok, new_facts, state} ->
          solve(requests, user, new_facts, strict_check_facts, state, strict_access?)
      end
    end
  end

  defp fetch_must_fetch(requests, state) do
    Enum.reduce_while(requests, {:ok, state}, fn request, {:ok, state} ->
      case Request.fetch_request_state(state, request) do
        {:ok, _state} ->
          {:cont, {:ok, state}}

        :error ->
          case request.fetcher.() do
            {:ok, value} ->
              {:cont, {:ok, Request.put_request_state(state, request, value)}}

            {:error, error} ->
              {:halt, {:error, error}}
          end
      end
    end)
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

  defp strict_check_facts(user, requests, strict_access?) do
    Enum.reduce(requests, %{true: true, false: false}, fn request, facts ->
      Ash.Authorization.Checker.strict_check(user, request, facts, strict_access?)
    end)
  end
end
