defmodule Ash.Authorization.SatSolver do
  alias Ash.Authorization.Clause

  @dialyzer {:no_return, :"picosat_solve/1"}

  def solve(requests, facts) do
    expression =
      Enum.reduce(requests, nil, fn request, expr ->
        requirements_expression =
          build_requirements_expression([request.rules], facts, request.filter)

        if expr do
          {:and, expr, requirements_expression}
        else
          requirements_expression
        end
      end)

    expression
    |> add_negations_and_solve()
    |> get_all_scenarios(expression)
    |> case do
      [] ->
        {:error, :unsatisfiable}

      scenarios ->
        {:ok,
         scenarios
         |> Enum.uniq()
         |> remove_irrelevant_clauses()}
    end
  end

  def strict_filter_subset(filter, candidate) do
    filter_expr = filter_to_expr(filter)
    candidate_expr = filter_to_expr(candidate)

    together = join_expr(filter_expr, candidate_expr, :and)

    separate = join_expr(negate(filter_expr), candidate_expr, :and)

    case solve_expression(together) do
      {:error, :unsatisfiable} ->
        false

      {:ok, _} ->
        case solve_expression(separate) do
          {:error, :unsatisfiable} ->
            true

          _ ->
            :maybe
        end
    end
  end

  defp negate(nil), do: nil
  defp negate(expr), do: {:not, expr}

  defp filter_to_expr(nil), do: nil
  defp filter_to_expr(%{impossible?: true}), do: false

  defp filter_to_expr(%{
         attributes: attributes,
         relationships: relationships,
         not: not_filter,
         ors: ors,
         path: path
       }) do
    expr =
      Enum.reduce(attributes, nil, fn {attr, statement}, expr ->
        join_expr(
          expr,
          tag_statement(statement_to_expr(statement), %{path: path, attr: attr}),
          :and
        )
      end)

    expr =
      Enum.reduce(relationships, expr, fn {relationship, relationship_filter}, expr ->
        join_expr(expr, {relationship, filter_to_expr(relationship_filter)}, :and)
      end)

    expr = join_expr(negate(filter_to_expr(not_filter)), expr, :and)

    Enum.reduce(ors, expr, fn or_filter, expr ->
      join_expr(filter_to_expr(or_filter), expr, :or)
    end)
  end

  defp statement_to_expr(%Ash.Filter.NotIn{values: values}) do
    {:not, %Ash.Filter.In{values: values}}
  end

  defp statement_to_expr(%Ash.Filter.NotEq{value: value}) do
    {:not, %Ash.Filter.Eq{value: value}}
  end

  defp statement_to_expr(%Ash.Filter.And{left: left, right: right}) do
    {:and, statement_to_expr(left), statement_to_expr(right)}
  end

  defp statement_to_expr(%Ash.Filter.Or{left: left, right: right}) do
    {:or, statement_to_expr(left), statement_to_expr(right)}
  end

  defp statement_to_expr(statement), do: statement

  defp tag_statement({:not, value}, tag), do: {:not, tag_statement(value, tag)}

  defp tag_statement({joiner, left_value, right_value}, tag) when joiner in [:and, :or],
    do: {joiner, tag_statement(left_value, tag), tag_statement(right_value, tag)}

  defp tag_statement(statement, tag), do: {statement, tag}

  defp join_expr(nil, right, _joiner), do: right
  defp join_expr(left, nil, _joiner), do: left
  defp join_expr(left, right, joiner), do: {joiner, left, right}

  defp get_all_scenarios(scenario_result, expression, scenarios \\ [])
  defp get_all_scenarios({:error, :unsatisfiable}, _, scenarios), do: scenarios

  defp get_all_scenarios({:ok, scenario}, expression, scenarios) do
    expression
    |> add_negations_and_solve([Map.drop(scenario, [true, false]) | scenarios])
    |> get_all_scenarios(expression, [Map.drop(scenario, [true, false]) | scenarios])
  end

  defp remove_irrelevant_clauses([scenario]), do: [scenario]

  defp remove_irrelevant_clauses(scenarios) do
    new_scenarios =
      scenarios
      |> Enum.uniq()
      |> Enum.map(fn scenario ->
        unnecessary_fact =
          Enum.find_value(scenario, fn
            {_fact, :unknowable} ->
              false

            # TODO: Is this acceptable?
            # If the check refers to empty data, and its meant to bypass strict checks
            # Then we consider that fact an irrelevant fact? Probably.
            {_fact, :irrelevant} ->
              true

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

  defp add_negations_and_solve(requirements_expression, negations \\ []) do
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

  def satsolver_solve(input) do
    Picosat.solve(input)
  end

  defp solve_expression(expression) do
    expression_with_constants = {:and, true, {:and, {:not, false}, expression}}

    {bindings, expression} = extract_bindings(expression_with_constants)

    expression
    |> to_conjunctive_normal_form()
    |> lift_clauses()
    |> negations_to_negative_numbers()
    |> satsolver_solve()
    |> solutions_to_predicate_values(bindings)
  end

  defp facts_to_statement(facts) do
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

  defp build_requirements_expression(sets_of_rules, facts, filter) do
    rules_expression =
      Enum.reduce(sets_of_rules, nil, fn rules, acc ->
        case acc do
          nil ->
            compile_rules_expression(rules, facts, filter)

          expr ->
            {:and, expr, compile_rules_expression(rules, facts, filter)}
        end
      end)

    facts =
      Enum.reduce(facts, %{}, fn {key, value}, acc ->
        if value == :unknowable do
          acc
        else
          Map.put(acc, key, value)
        end
      end)

    facts_expression = facts_to_statement(Map.drop(facts, [true, false]))

    if facts_expression do
      {:and, facts_expression, rules_expression}
    else
      rules_expression
    end
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

  defp compile_rules_expression([], _facts, _filter) do
    true
  end

  defp compile_rules_expression([{:authorize_if, clause}], facts, filter) do
    clause = %{clause | filter: filter}

    case Clause.find(facts, clause) do
      {:ok, true} -> true
      {:ok, false} -> false
      {:ok, :unknowable} -> false
      {:ok, :irrelevant} -> true
      :error -> clause
    end
  end

  defp compile_rules_expression([{:authorize_if, clause} | rest], facts, filter) do
    clause = %{clause | filter: filter}

    case Clause.find(facts, clause) do
      {:ok, true} ->
        true

      {:ok, false} ->
        compile_rules_expression(rest, facts, filter)

      {:ok, :irrelevant} ->
        true

      {:ok, :unknowable} ->
        compile_rules_expression(rest, facts, filter)

      :error ->
        {:or, clause, compile_rules_expression(rest, facts, filter)}
    end
  end

  defp compile_rules_expression([{:authorize_unless, clause}], facts, filter) do
    clause = %{clause | filter: filter}

    case Clause.find(facts, clause) do
      {:ok, true} ->
        false

      {:ok, false} ->
        true

      {:ok, :irrelevant} ->
        true

      {:ok, :unknowable} ->
        false

      :error ->
        {:not, clause}
    end
  end

  defp compile_rules_expression([{:authorize_unless, clause} | rest], facts, filter) do
    clause = %{clause | filter: filter}

    case Clause.find(facts, clause) do
      {:ok, true} ->
        compile_rules_expression(rest, facts, filter)

      {:ok, false} ->
        true

      {:ok, :irrelevant} ->
        true

      {:ok, :unknowable} ->
        compile_rules_expression(rest, facts, filter)

      :error ->
        {:or, {:not, clause}, compile_rules_expression(rest, facts, filter)}
    end
  end

  defp compile_rules_expression([{:forbid_if, _clause}], _facts, _) do
    false
  end

  defp compile_rules_expression([{:forbid_if, clause} | rest], facts, filter) do
    clause = %{clause | filter: filter}

    case Clause.find(facts, clause) do
      {:ok, true} ->
        false

      {:ok, :irrelevant} ->
        compile_rules_expression(rest, facts, filter)

      {:ok, :unknowable} ->
        false

      {:ok, false} ->
        compile_rules_expression(rest, facts, filter)

      :error ->
        {:and, {:not, clause}, compile_rules_expression(rest, facts, filter)}
    end
  end

  defp compile_rules_expression([{:forbid_unless, _clause}], _facts, _id) do
    false
  end

  defp compile_rules_expression([{:forbid_unless, clause} | rest], facts, filter) do
    clause = %{clause | filter: filter}

    case Clause.find(facts, clause) do
      {:ok, true} ->
        compile_rules_expression(rest, facts, filter)

      {:ok, false} ->
        false

      {:ok, :irrelevant} ->
        false

      {:ok, :unknowable} ->
        false

      :error ->
        {:and, clause, compile_rules_expression(rest, facts, filter)}
    end
  end

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

  # {:and, {:or, 1, 2}, {:and, {:or, 3, 4}, {:or, 5, 6}}}

  # [[1, 2], [3]]

  # TODO: Is this so simple?
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
