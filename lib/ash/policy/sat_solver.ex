defmodule Ash.Policy.SatSolver do
  @moduledoc false
  def solve(expression) do
    expression
    |> add_negations_and_solve([])
    |> get_all_scenarios(expression)
    |> case do
      [] ->
        {:error, :unsatisfiable}

      scenarios ->
        static_checks = [
          {Ash.Policy.Check.Static, [result: true]},
          {Ash.Policy.Check.Static, [result: false]}
        ]

        {:ok,
         scenarios
         |> Enum.map(&Map.drop(&1, static_checks))
         |> Enum.uniq()}
    end
  end

  defp get_all_scenarios(scenario_result, expression, scenarios \\ [])
  defp get_all_scenarios({:error, :unsatisfiable}, _, scenarios), do: scenarios

  defp get_all_scenarios({:ok, scenario}, expression, scenarios) do
    expression
    |> add_negations_and_solve([Map.drop(scenario, [true, false]) | scenarios])
    |> get_all_scenarios(expression, [Map.drop(scenario, [true, false]) | scenarios])
  end

  def simplify_clauses([scenario]), do: [scenario]

  def simplify_clauses(scenarios) do
    scenarios
    |> Enum.map(fn scenario ->
      scenario
      |> Enum.flat_map(fn {fact, value} ->
        if Enum.find(scenarios, fn other_scenario ->
             other_scenario != scenario &&
               Map.delete(other_scenario, fact) == Map.delete(scenario, fact) &&
               Map.fetch(other_scenario, fact) == {:ok, !value}
           end) do
          [fact]
        else
          []
        end
      end)
      |> case do
        [] ->
          scenario

        facts ->
          Map.drop(scenario, facts)
      end
    end)
    |> Enum.uniq()
    |> case do
      ^scenarios ->
        scenarios

      new_scenarios ->
        simplify_clauses(new_scenarios)
    end
  end

  def scenario_makes_fact_irrelevant?(potential_irrelevant_maker, scenario, fact) do
    (Map.delete(potential_irrelevant_maker, fact) ==
       Map.delete(scenario, fact) &&
       Map.has_key?(potential_irrelevant_maker, fact) && Map.has_key?(scenario, fact) &&
       Map.get(potential_irrelevant_maker, fact) !=
         Map.get(scenario, fact)) ||
      (!Map.has_key?(potential_irrelevant_maker, fact) &&
         scenario_is_subset?(potential_irrelevant_maker, scenario))
  end

  defp scenario_is_subset?(left, right) do
    Enum.all?(left, fn {fact, value} ->
      Map.get(right, fact) == value
    end)
  end

  @spec add_negations_and_solve(term, term) :: term | no_return()
  defp add_negations_and_solve(requirements_expression, negations) do
    negations =
      Enum.reduce(negations, nil, fn negation, expr ->
        negation_statement =
          negation
          |> Map.drop([true, false])
          |> facts_to_statement()

        if expr do
          {:and, expr, {:not, negation_statement}}
        else
          {:not, negation_statement}
        end
      end)

    full_expression =
      if negations do
        {:and, requirements_expression, negations}
      else
        requirements_expression
      end

    solve_expression(full_expression)
  end

  def facts_to_statement(facts) do
    Enum.reduce(facts, nil, fn {fact, true?}, expr ->
      expr_component =
        if true? do
          fact
        else
          {:not, fact}
        end

      if expr do
        {:and, expr, expr_component}
      else
        expr_component
      end
    end)
  end

  defp solve_expression(expression) do
    Ash.SatSolver.solve_expression(expression)
  end
end
