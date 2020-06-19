defmodule Ash.SatSolver do
  @moduledoc false

  alias Ash.Filter
  alias Ash.Filter.{Expression, Not, Predicate}

  def strict_filter_subset(filter, candidate) do
    case {filter, candidate} do
      {%{expression: nil}, %{expression: nil}} ->
        true

      {%{expression: nil}, _candidate_expr} ->
        true

      {_filter_expr, %{expression: nil}} ->
        false

      {filter, candidate} ->
        do_strict_filter_subset(filter, candidate)
    end
  end

  defp do_strict_filter_subset(filter, candidate) do
    case add_comparisons_and_solve_expression(
           Expression.new(:and, filter.expression, candidate.expression)
         ) do
      {:error, :unsatisfiable} ->
        false

      {:ok, _} ->
        case add_comparisons_and_solve_expression(
               Expression.new(:and, Not.new(filter.expression), candidate.expression)
             ) do
          {:error, :unsatisfiable} ->
            true

          _ ->
            :maybe
        end
    end
  end

  defp filter_to_expr(nil), do: nil
  defp filter_to_expr(false), do: false
  defp filter_to_expr(true), do: true
  defp filter_to_expr(%Filter{expression: expression}), do: filter_to_expr(expression)
  defp filter_to_expr(%Predicate{} = predicate), do: predicate
  defp filter_to_expr(%Not{expression: expression}), do: {:not, filter_to_expr(expression)}

  defp filter_to_expr(%Expression{op: op, left: left, right: right}) do
    {op, filter_to_expr(left), filter_to_expr(right)}
  end

  def add_comparisons_and_solve_expression(expression) do
    all_predicates =
      Filter.reduce(expression, [], fn
        %Predicate{} = predicate, predicates ->
          [predicate | predicates]

        _, predicates ->
          predicates
      end)

    simplified =
      Filter.map(expression, fn
        %Predicate{} = predicate ->
          predicate
          |> find_simplification(all_predicates)
          |> case do
            nil ->
              predicate

            {:simplify, simplification} ->
              simplification
          end

        other ->
          other
      end)

    if simplified == expression do
      all_predicates =
        Filter.reduce(expression, [], fn
          %Predicate{} = predicate, predicates ->
            [predicate | predicates]

          _, predicates ->
            predicates
        end)
        |> Enum.uniq()

      comparison_expressions =
        all_predicates
        |> Enum.reduce([], fn predicate, new_expressions ->
          all_predicates
          |> Enum.filter(fn other_predicate ->
            other_predicate != predicate &&
              other_predicate.relationship_path == predicate.relationship_path &&
              other_predicate.attribute.name == predicate.attribute.name
          end)
          |> Enum.reduce(new_expressions, fn other_predicate, new_expressions ->
            case Predicate.compare(predicate, other_predicate) do
              inclusive when inclusive in [:right_includes_left, :mutually_inclusive] ->
                [{:not, {:and, {:not, other_predicate}, predicate}} | new_expressions]

              exclusive when exclusive in [:right_excludes_left, :mutually_exclusive] ->
                [{:not, {:and, other_predicate, predicate}} | new_expressions]

              {:simplify, _} ->
                # Filter should be fully simplified here
                raise "What"

              _other ->
                # If we can't tell, we assume they are exclusive statements
                [{:not, {:and, other_predicate, predicate}} | new_expressions]
            end
          end)
        end)
        |> Enum.uniq()

      expression = filter_to_expr(expression)

      expression =
        Enum.reduce(comparison_expressions, expression, fn comparison_expression, expression ->
          {:and, comparison_expression, expression}
        end)

      solve_expression(expression)
    else
      add_comparisons_and_solve_expression(simplified)
    end
  end

  defp find_simplification(predicate, predicates) do
    predicates
    |> Enum.find_value(fn other_predicate ->
      case Predicate.compare(predicate, other_predicate) do
        {:simplify, simplification} -> {:simplify, simplification}
        _ -> false
      end
    end)
  end

  def solve_expression(expression) do
    expression_with_constants = {:and, true, {:and, {:not, false}, expression}}

    {bindings, expression} = extract_bindings(expression_with_constants)

    expression
    |> to_conjunctive_normal_form()
    |> lift_clauses()
    |> negations_to_negative_numbers()
    |> Picosat.solve()
    |> solutions_to_predicate_values(bindings)
  end

  defp solutions_to_predicate_values({:ok, solution}, bindings) do
    scenario =
      Enum.reduce(solution, %{true: [], false: []}, fn var, state ->
        fact = Map.get(bindings, abs(var))

        Map.put(state, fact, var > 0)
      end)

    {:ok, scenario}
  end

  defp solutions_to_predicate_values({:error, error}, _), do: {:error, error}

  defp extract_bindings(expr, bindings \\ %{current: 1})

  defp extract_bindings({operator, left, right}, bindings) do
    {bindings, left_extracted} = extract_bindings(left, bindings)
    {bindings, right_extracted} = extract_bindings(right, bindings)

    {bindings, {operator, left_extracted, right_extracted}}
  end

  defp extract_bindings({:not, value}, bindings) do
    {bindings, extracted} = extract_bindings(value, bindings)

    {bindings, {:not, extracted}}
  end

  defp extract_bindings(value, %{current: current} = bindings) do
    current_binding =
      Enum.find(bindings, fn {key, binding_value} ->
        key != :current && binding_value == value
      end)

    case current_binding do
      nil ->
        new_bindings =
          bindings
          |> Map.put(:current, current + 1)
          |> Map.put(current, value)

        {new_bindings, current}

      {binding, _} ->
        {bindings, binding}
    end
  end

  # A helper function for formatting to the same output we'd give to picosat
  @doc false
  def to_picosat(clauses, variable_count) do
    clause_count = Enum.count(clauses)

    formatted_input =
      Enum.map_join(clauses, "\n", fn clause ->
        format_clause(clause) <> " 0"
      end)

    "p cnf #{variable_count} #{clause_count}\n" <> formatted_input
  end

  defp negations_to_negative_numbers(clauses) do
    Enum.map(
      clauses,
      fn
        {:not, var} when is_integer(var) ->
          [negate_var(var)]

        var when is_integer(var) ->
          [var]

        clause ->
          Enum.map(clause, fn
            {:not, var} -> negate_var(var)
            var -> var
          end)
      end
    )
  end

  defp negate_var(var, multiplier \\ -1)

  defp negate_var({:not, value}, multiplier) do
    negate_var(value, multiplier * -1)
  end

  defp negate_var(value, multiplier), do: value * multiplier

  defp format_clause(clause) do
    Enum.map_join(clause, " ", fn
      {:not, var} -> "-#{var}"
      var -> "#{var}"
    end)
  end

  defp lift_clauses({:and, left, right}) do
    lift_clauses(left) ++ lift_clauses(right)
  end

  defp lift_clauses({:or, left, right}) do
    [lift_or_clauses(left) ++ lift_or_clauses(right)]
  end

  defp lift_clauses(value), do: [[value]]

  defp lift_or_clauses({:or, left, right}) do
    lift_or_clauses(left) ++ lift_or_clauses(right)
  end

  defp lift_or_clauses(value), do: [value]

  defp to_conjunctive_normal_form(expression) do
    expression
    |> demorgans_law()
    |> distributive_law()
  end

  defp distributive_law(expression) do
    distributive_law_applied = apply_distributive_law(expression)

    if expression == distributive_law_applied do
      expression
    else
      distributive_law(distributive_law_applied)
    end
  end

  defp apply_distributive_law({:or, left, {:and, right1, right2}}) do
    left_distributed = apply_distributive_law(left)

    {:and, {:or, left_distributed, apply_distributive_law(right1)},
     {:or, left_distributed, apply_distributive_law(right2)}}
  end

  defp apply_distributive_law({:or, {:and, left1, left2}, right}) do
    right_distributed = apply_distributive_law(right)

    {:and, {:or, apply_distributive_law(left1), right_distributed},
     {:or, apply_distributive_law(left2), right_distributed}}
  end

  defp apply_distributive_law({:not, expression}) do
    {:not, apply_distributive_law(expression)}
  end

  defp apply_distributive_law({operator, left, right}) when operator in [:and, :or] do
    {operator, apply_distributive_law(left), apply_distributive_law(right)}
  end

  defp apply_distributive_law(var) when is_integer(var) do
    var
  end

  defp demorgans_law(expression) do
    demorgans_law_applied = apply_demorgans_law(expression)

    if expression == demorgans_law_applied do
      expression
    else
      demorgans_law(demorgans_law_applied)
    end
  end

  defp apply_demorgans_law({:not, {:and, left, right}}) do
    {:or, {:not, apply_demorgans_law(left)}, {:not, apply_demorgans_law(right)}}
  end

  defp apply_demorgans_law({:not, {:or, left, right}}) do
    {:and, {:not, left}, {:not, right}}
  end

  defp apply_demorgans_law({operator, left, right}) when operator in [:or, :and] do
    {operator, apply_demorgans_law(left), apply_demorgans_law(right)}
  end

  defp apply_demorgans_law({:not, expression}) do
    {:not, apply_demorgans_law(expression)}
  end

  defp apply_demorgans_law(var) when is_integer(var) do
    var
  end
end
