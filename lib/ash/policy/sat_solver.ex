defmodule Ash.Policy.SatSolver do
  @moduledoc false
  def solve(expression, mapper \\ nil) do
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
    unnecessary_clauses =
      scenarios
      |> Enum.with_index()
      |> Enum.flat_map(fn {scenario, index} ->
        scenario
        |> Enum.flat_map(fn {fact, _value} ->
          if Enum.find(scenarios, fn other_scenario ->
               scenario_makes_fact_irrelevant?(other_scenario, scenario, fact)
             end) do
            [fact]
          else
            []
          end
        end)
        |> Enum.map(fn fact ->
          {index, fact}
        end)
      end)
      |> Enum.group_by(&elem(&1, 0), &elem(&1, 1))

    case unnecessary_clauses do
      empty when empty == %{} ->
        scenarios

      unnecessary_clauses ->
        unnecessary_clauses
        |> Enum.reduce(scenarios, fn {index, facts}, scenarios ->
          List.update_at(scenarios, index, &Map.drop(&1, facts))
        end)
        |> Enum.reject(&(&1 == %{}))
        |> Enum.uniq()
        |> simplify_clauses()
    end
  end

  def scenario_makes_fact_irrelevant?(potential_irrelevant_maker, _scenario, _fact)
      when potential_irrelevant_maker == %{},
      do: false

  def scenario_makes_fact_irrelevant?(potential_irrelevant_maker, scenario, fact) do
    scenario_is_subset?(Map.delete(potential_irrelevant_maker, fact), scenario) &&
      Map.has_key?(potential_irrelevant_maker, fact) && Map.has_key?(scenario, fact) &&
      Map.get(potential_irrelevant_maker, fact) !=
        Map.get(scenario, fact)
  end

  defp scenario_is_subset?(left, right) do
    Enum.all?(left, fn {fact, value} ->
      Map.get(right, fact) == value
    end)
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
