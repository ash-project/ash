defmodule Ash.Authorization.SatSolver do
  def solve(authorization_steps) do
    authorization_steps_expression = compile_authorization_steps_expression(authorization_steps)

    {bindings, expression} = extract_bindings(authorization_steps_expression)

    expression
    |> to_conjunctive_normal_form()
    |> lift_clauses()
    |> negations_to_negative_numbers()
    |> Picosat.solve()
    |> solutions_to_predicate_values(bindings)
  end

  defp solutions_to_predicate_values({:ok, solution}, bindings) do
    Enum.reduce(solution, %{}, fn var, state ->
      default_value_state = %{
        require_true: [],
        require_false: []
      }

      bindings
      |> Map.get(abs(var))
      |> Enum.reduce(state, fn {key_or_relationship, filter}, state ->
        state
        |> Map.put_new(key_or_relationship, default_value_state)
        |> Map.update!(key_or_relationship, fn filter_state ->
          key =
            if var < 0 do
              :require_true
            else
              :require_false
            end

          Map.update!(filter_state, key, fn current_filters_for_key ->
            [filter | current_filters_for_key]
          end)
        end)
      end)
    end)

    # format this in a queryable way? What to do here? PTFO, thats what

    # Enum.reduce(solution, %{}, fn solution, acc ->
    #   filter = Map.get(bindings, abs(solution))

    #   Enum.reduce(filter, acc, fn {key, value}, acc ->
    #     Map.update(acc, key, [{required_value, value}], fn statements ->
    #       Keyword.update(statements, required_value, [value], fn existing_values ->
    #         [value | existing_values]
    #       end)
    #     end)
    #   end)
    # end)
  end

  defp solutions_to_predicate_values({:error, error}, _), do: {:error, error}

  # Is it really this easy
  defp compile_authorization_steps_expression([{:authorize_if, clause}]) do
    clause
  end

  defp compile_authorization_steps_expression([{:authorize_if, clause} | rest]) do
    {:or, clause, compile_authorization_steps_expression(rest)}
  end

  defp compile_authorization_steps_expression([{:authorize_unless, clause}]) do
    {:not, clause}
  end

  defp compile_authorization_steps_expression([{:authorize_unless, clause} | rest]) do
    {:or, {:not, clause}, compile_authorization_steps_expression(rest)}
  end

  defp compile_authorization_steps_expression([{:forbid_if, clause}]) do
    {:not, clause}
  end

  defp compile_authorization_steps_expression([{:forbid_if, clause} | rest]) do
    {:and, {:not, clause}, compile_authorization_steps_expression(rest)}
  end

  defp compile_authorization_steps_expression([{:forbid_unless, clause}]) do
    clause
  end

  defp compile_authorization_steps_expression([{:forbid_unless, clause} | rest]) do
    {:and, clause, compile_authorization_steps_expression(rest)}
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
    new_bindings =
      bindings
      |> Map.put(:current, current + 1)
      |> Map.put(current, value)

    {new_bindings, current}
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
    Enum.map(clauses, fn clause ->
      if is_integer(clause) do
        [clause]
      else
        Enum.map(clause, fn
          {:not, var} -> -var
          var -> var
        end)
      end
    end)
  end

  defp format_clause(clause) do
    Enum.map_join(clause, " ", fn
      {:not, var} -> "-#{var}"
      var -> "#{var}"
    end)
  end

  defp lift_clauses({:and, {:and, _, _} = left, {:and, _, _} = right}) do
    lift_clauses(left) ++ lift_clauses(right)
  end

  defp lift_clauses({:and, {:and, _, _} = left, right}) do
    [[lift_clauses(left)]] ++ lift_clauses(right)
  end

  defp lift_clauses({:and, left, {:and, _, _} = right}) do
    [[lift_clauses(right)]] ++ lift_clauses(left)
  end

  defp lift_clauses({:and, left, right}) do
    [lift_clauses(left), lift_clauses(right)]
  end

  defp lift_clauses({:or, {:or, _, _} = left, {:or, _, _} = right}) do
    lift_clauses(left) ++ lift_clauses(right)
  end

  defp lift_clauses({:or, {:or, _, _} = left, right}) do
    [right | lift_clauses(left)]
  end

  defp lift_clauses({:or, left, {:or, _, _} = right}) do
    [left | lift_clauses(right)]
  end

  defp lift_clauses({:or, left, right}) do
    [left, right]
  end

  defp lift_clauses({:not, var}) do
    [{:not, var}]
  end

  defp lift_clauses(var) do
    [var]
  end

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
