defmodule Ash.Authorization.SatSolver do
  def solve(sets_of_authorization_steps, facts, negations \\ []) do
    authorization_steps_expression =
      Enum.reduce(sets_of_authorization_steps, nil, fn authorization_steps, acc ->
        case acc do
          nil ->
            compile_authorization_steps_expression(authorization_steps, facts)

          expr ->
            {:and, expr, compile_authorization_steps_expression(authorization_steps, facts)}
        end
      end)

    facts_expression = facts_to_statement(facts)

    negations =
      Enum.reduce(negations, nil, fn negation, expr ->
        negation_statement = facts_to_statement(negation)

        if expr do
          {:and, expr, {:not, negation_statement}}
        else
          {:not, negation_statement}
        end
      end)

    authorization_steps_with_facts =
      if facts_expression do
        {:and, facts_expression, authorization_steps_expression}
      else
        authorization_steps_expression
      end

    full_expression =
      if negations do
        {:and, authorization_steps_with_facts, negations}
      else
        authorization_steps_with_facts
      end

    {bindings, expression} = extract_bindings(full_expression)

    expression
    |> to_conjunctive_normal_form()
    |> lift_clauses()
    |> negations_to_negative_numbers()
    |> picosat_solve()
    |> solutions_to_predicate_values(bindings)
  end

  defp picosat_solve(equation) do
    Picosat.solve(equation)
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

  defp solutions_to_predicate_values({:ok, solution}, bindings) do
    scenario =
      Enum.reduce(solution, %{true: [], false: []}, fn var, state ->
        fact = Map.get(bindings, abs(var))

        Map.put(state, fact, var > 0)
      end)

    {:ok, scenario}
  end

  defp solutions_to_predicate_values({:error, error}, _), do: {:error, error}

  # Is it really this easy
  defp compile_authorization_steps_expression([{:authorize_if, clause}], facts) do
    case Map.fetch(facts, clause) do
      {:ok, true} -> true
      {:ok, false} -> false
      :error -> clause
    end
  end

  defp compile_authorization_steps_expression([{:authorize_if, clause} | rest], facts) do
    case Map.fetch(facts, clause) do
      {:ok, true} ->
        true

      {:ok, false} ->
        compile_authorization_steps_expression(rest, facts)

      :error ->
        {:or, clause, compile_authorization_steps_expression(rest, facts)}
    end
  end

  defp compile_authorization_steps_expression([{:authorize_unless, clause}], facts) do
    case Map.fetch(facts, clause) do
      {:ok, true} -> false
      {:ok, false} -> true
      :error -> {:not, clause}
    end
  end

  defp compile_authorization_steps_expression([{:authorize_unless, clause} | rest], facts) do
    case Map.fetch(facts, clause) do
      {:ok, true} -> compile_authorization_steps_expression(rest, facts)
      {:ok, false} -> true
      :error -> {:or, {:not, clause}, compile_authorization_steps_expression(rest, facts)}
    end
  end

  defp compile_authorization_steps_expression([{:forbid_if, clause}], facts) do
    case Map.fetch(facts, clause) do
      {:ok, true} -> false
      {:ok, false} -> true
      :error -> {:not, clause}
    end
  end

  defp compile_authorization_steps_expression([{:forbid_if, clause} | rest], facts) do
    case Map.fetch(facts, clause) do
      {:ok, true} -> false
      {:ok, false} -> compile_authorization_steps_expression(rest, facts)
      :error -> {:and, {:not, clause}, compile_authorization_steps_expression(rest, facts)}
    end
  end

  defp compile_authorization_steps_expression([{:forbid_unless, clause}], facts) do
    case Map.fetch(facts, clause) do
      {:ok, true} -> true
      {:ok, false} -> false
      :error -> clause
    end
  end

  defp compile_authorization_steps_expression([{:forbid_unless, clause} | rest], facts) do
    case Map.fetch(facts, clause) do
      {:ok, true} -> compile_authorization_steps_expression(rest, facts)
      {:ok, false} -> false
      :error -> {:and, clause, compile_authorization_steps_expression(rest, facts)}
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
