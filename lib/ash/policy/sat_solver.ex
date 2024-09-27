defmodule Ash.Policy.SatSolver do
  @moduledoc false
  def solve(expression, mapper \\ nil)
  def solve(false, _), do: {:error, :unsatisfiable}

  def solve(true, mapper) do
    if mapper do
      {:ok, Enum.map([%{}], fn scenario -> mapper.(scenario, %{}) end)}
    else
      {:ok, [%{}]}
    end
  end

  def solve(expression, mapper) do
    {cnf, bindings} = Ash.SatSolver.to_cnf(expression)

    cnf
    |> add_negations_and_solve([])
    |> get_all_scenarios(cnf)
    |> case do
      [] ->
        {:error, :unsatisfiable}

      scenarios ->
        {scenarios, bindings} = Ash.SatSolver.unbind(scenarios, bindings)

        if mapper do
          {:ok,
           Enum.map(scenarios, fn scenario ->
             mapper.(scenario, bindings)
           end)}
        else
          {:ok, scenarios}
        end
    end
  end

  defp get_all_scenarios(
         scenario_result,
         expression,
         scenarios \\ [],
         negations \\ []
       )

  defp get_all_scenarios(
         {:error, :unsatisfiable},
         _,
         scenarios,
         _negations
       ),
       do: scenarios

  defp get_all_scenarios({:ok, scenario}, expression, scenarios, negations) do
    all_scenarios = [scenario | scenarios]
    negations = [Enum.map(scenario, &(-&1)) | negations]

    expression
    |> add_negations_and_solve(negations)
    |> get_all_scenarios(expression, all_scenarios, negations)
  end

  def simplify_clauses([scenario]), do: [scenario]

  def simplify_clauses(scenarios) do
    indexed = Enum.with_index(scenarios)

    indexed
    |> Enum.find_value(fn {scenario, index} ->
      Enum.find_value(scenario, fn {fact, _value} ->
        case Enum.find_value(indexed, fn {other_scenario, other_index} ->
               if scenario != other_scenario &&
                    scenario_makes_fact_irrelevant?(other_scenario, scenario, fact) do
                 other_index
               end
             end) do
          nil ->
            nil

          other_index ->
            {fact, other_index, index}
        end
      end)
    end)
    |> case do
      nil ->
        scenarios

      {fact, index1, index2} ->
        scenarios
        |> List.update_at(index1, &Map.delete(&1, fact))
        |> List.update_at(index2, &Map.delete(&1, fact))
        |> Enum.uniq()
        |> simplify_clauses()
    end
  end

  def scenario_makes_fact_irrelevant?(potential_irrelevant_maker, _scenario, _fact)
      when potential_irrelevant_maker == %{},
      do: false

  def scenario_makes_fact_irrelevant?(potential_irrelevant_maker, scenario, fact) do
    Map.delete(potential_irrelevant_maker, fact) == Map.delete(scenario, fact) &&
      Map.has_key?(potential_irrelevant_maker, fact) && Map.has_key?(scenario, fact) &&
      Map.get(potential_irrelevant_maker, fact) !=
        Map.get(scenario, fact)
  end

  @spec add_negations_and_solve(term, term) :: term | no_return()
  defp add_negations_and_solve(cnf, negations) do
    solve_expression(cnf ++ negations)
  end

  def facts_to_statement(facts) do
    Enum.reduce(facts, nil, fn
      {_fact, :unknown}, expr ->
        expr

      {fact, true?}, expr ->
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
