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
         |> Enum.uniq()
         |> remove_irrelevant_clauses()
         |> Enum.uniq()
         |> Enum.map(&Map.drop(&1, static_checks))}
    end
  end

  defp get_all_scenarios(scenario_result, expression, scenarios \\ [])
  defp get_all_scenarios({:error, :unsatisfiable}, _, scenarios), do: scenarios

  defp get_all_scenarios({:ok, scenario}, expression, scenarios) do
    expression
    |> add_negations_and_solve([Map.drop(scenario, [true, false]) | scenarios])
    |> get_all_scenarios(expression, [Map.drop(scenario, [true, false]) | scenarios])
  end

  def remove_irrelevant_clauses([scenario]), do: [scenario]

  def remove_irrelevant_clauses(scenarios) do
    new_scenarios =
      scenarios
      |> Enum.uniq()
      |> Enum.map(fn scenario ->
        unnecessary_fact = find_unnecessary_fact(scenario, scenarios)

        Map.delete(scenario, unnecessary_fact)
      end)
      |> Enum.uniq()

    if new_scenarios == scenarios do
      scenarios
    else
      remove_irrelevant_clauses(new_scenarios)
    end
  end

  defp find_unnecessary_fact(scenario, scenarios) do
    Enum.find_value(scenario, fn
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
