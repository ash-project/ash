defmodule Ash.SatSolver do
  @moduledoc false
  @dialyzer {:nowarn_function, strict_filter_subset: 2}
  @dialyzer {:nowarn_function, solve_expression: 1}
  @dialyzer {:nowarn_function, solutions_to_predicate_values: 2}

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
         ands: ands,
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

    expr =
      Enum.reduce(ors, expr, fn or_filter, expr ->
        join_expr(filter_to_expr(or_filter), expr, :or)
      end)

    Enum.reduce(ands, expr, fn and_filter, expr ->
      join_expr(filter_to_expr(and_filter), expr, :and)
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

  defp solve_expression(expression) do
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
