defmodule Ash.SatSolver do
  @moduledoc false

  alias Ash.Filter
  alias Ash.Query.{BooleanExpression, Not, Ref}

  @dialyzer {:nowarn_function, overlap?: 2}

  defmacro b(statement) do
    value =
      Macro.prewalk(
        statement,
        fn
          {:and, _, [left, right]} ->
            quote do
              {:and, unquote(left), unquote(right)}
            end

          {:or, _, [left, right]} ->
            quote do
              {:or, unquote(left), unquote(right)}
            end

          {:not, _, [value]} ->
            quote do
              {:not, unquote(value)}
            end

          other ->
            other
        end
      )

    quote do
      unquote(value)
      |> Ash.SatSolver.balance()
    end
  end

  def balance({op, left, right}) do
    left = balance(left)
    right = balance(right)
    [left, right] = Enum.sort([left, right])

    {op, left, right}
  end

  def balance({:not, {:not, right}}) do
    balance(right)
  end

  def balance({:not, statement}) do
    {:not, balance(statement)}
  end

  def balance(other), do: other

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
    case transform_and_solve(
           filter.resource,
           BooleanExpression.new(:and, filter.expression, candidate.expression)
         ) do
      {:error, :unsatisfiable} ->
        false

      {:ok, _} ->
        case transform_and_solve(
               filter.resource,
               BooleanExpression.new(:and, Not.new(filter.expression), candidate.expression)
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
  defp filter_to_expr(%{__predicate__?: true} = predicate), do: predicate
  defp filter_to_expr(%Not{expression: expression}), do: b(not filter_to_expr(expression))

  defp filter_to_expr(%BooleanExpression{op: op, left: left, right: right}) do
    {op, filter_to_expr(left), filter_to_expr(right)}
  end

  def transform_and_solve(resource, expression) do
    expression
    |> consolidate_relationships(resource)
    |> upgrade_related_filters_to_join_keys(resource)
    |> build_expr_with_predicate_information()
    |> solve_expression()
  end

  defp upgrade_related_filters_to_join_keys(
         %BooleanExpression{op: op, left: left, right: right},
         resource
       ) do
    BooleanExpression.new(
      op,
      upgrade_related_filters_to_join_keys(left, resource),
      upgrade_related_filters_to_join_keys(right, resource)
    )
  end

  defp upgrade_related_filters_to_join_keys(%Not{expression: expression}, resource) do
    Not.new(upgrade_related_filters_to_join_keys(expression, resource))
  end

  defp upgrade_related_filters_to_join_keys(
         %{__operator__?: true, left: left, right: right} = op,
         resource
       ) do
    %{op | left: upgrade_ref(left, resource), right: upgrade_ref(right, resource)}
  end

  defp upgrade_related_filters_to_join_keys(
         %{__function__?: true, arguments: arguments} = function,
         resource
       ) do
    %{function | arguments: Enum.map(arguments, &upgrade_ref(&1, resource))}
  end

  defp upgrade_related_filters_to_join_keys(expr, _), do: expr

  defp upgrade_ref(
         %Ash.Query.Ref{attribute: attribute, relationship_path: path} = ref,
         resource
       )
       when path != [] do
    with relationship when not is_nil(relationship) <-
           Ash.Resource.Info.relationship(resource, path),
         true <- attribute.name == relationship.destination_field,
         new_attribute when not is_nil(new_attribute) <-
           Ash.Resource.Info.attribute(relationship.source, relationship.source_field) do
      %{
        ref
        | relationship_path: :lists.droplast(path),
          attribute: new_attribute,
          resource: resource
      }
    else
      _ ->
        ref
    end
  end

  defp upgrade_ref(other, _), do: other

  defp consolidate_relationships(expression, resource) do
    {replacements, _all_relationship_paths} =
      expression
      |> Filter.relationship_paths()
      |> Enum.reduce({%{}, []}, fn path, {replacements, kept_paths} ->
        case find_synonymous_relationship_path(resource, kept_paths, path) do
          nil ->
            {replacements, [path | kept_paths]}

          synonymous_path ->
            Map.put(replacements, path, synonymous_path)
        end
      end)

    do_consolidate_relationships(expression, replacements)
  end

  defp do_consolidate_relationships(
         %BooleanExpression{op: op, left: left, right: right},
         replacements
       ) do
    BooleanExpression.new(
      op,
      do_consolidate_relationships(left, replacements),
      do_consolidate_relationships(right, replacements)
    )
  end

  defp do_consolidate_relationships(%Not{expression: expression}, replacements) do
    Not.new(do_consolidate_relationships(expression, replacements))
  end

  defp do_consolidate_relationships(%Ash.Query.Ref{relationship_path: path} = ref, replacements)
       when path != [] do
    case Map.fetch(replacements, path) do
      {:ok, replacement} when not is_nil(replacement) -> %{ref | relationship_path: replacement}
      :error -> ref
    end
  end

  defp do_consolidate_relationships(%{__function__?: true, arguments: args} = func, replacements) do
    %{func | arguments: Enum.map(args, &do_consolidate_relationships(&1, replacements))}
  end

  defp do_consolidate_relationships(
         %{__operator__?: true, left: left, right: right} = op,
         replacements
       ) do
    %{
      op
      | left: do_consolidate_relationships(left, replacements),
        right: do_consolidate_relationships(right, replacements)
    }
  end

  defp do_consolidate_relationships(other, _), do: other

  defp find_synonymous_relationship_path(resource, paths, path) do
    Enum.find_value(paths, fn candidate_path ->
      if synonymous_relationship_paths?(resource, candidate_path, path) do
        candidate_path
      else
        false
      end
    end)
  end

  def synonymous_relationship_paths?(_, [], []), do: true

  def synonymous_relationship_paths?(_resource, candidate_path, path)
      when length(candidate_path) != length(path),
      do: false

  def synonymous_relationship_paths?(resource, [candidate_first | candidate_rest], [first | rest])
      when first == candidate_first do
    synonymous_relationship_paths?(
      Ash.Resource.Info.relationship(resource, candidate_first).destination,
      candidate_rest,
      rest
    )
  end

  def synonymous_relationship_paths?(
        left_resource,
        candidate,
        search,
        right_resource \\ nil
      )

  def synonymous_relationship_paths?(_, [], [], _), do: true
  def synonymous_relationship_paths?(_, [], _, _), do: false
  def synonymous_relationship_paths?(_, _, [], _), do: false

  def synonymous_relationship_paths?(
        left_resource,
        [candidate_first | candidate_rest] = candidate,
        [first | rest] = search,
        right_resource
      ) do
    right_resource = right_resource || left_resource
    relationship = Ash.Resource.Info.relationship(left_resource, first)
    candidate_relationship = Ash.Resource.Info.relationship(right_resource, candidate_first)

    cond do
      !relationship || !candidate_relationship ->
        false

      relationship.type == :many_to_many && candidate_relationship.type == :has_many ->
        synonymous_relationship_paths?(
          left_resource,
          [relationship.join_relationship | candidate],
          search,
          right_resource
        )

      relationship.type == :has_many && candidate_relationship.type == :many_to_many ->
        synonymous_relationship_paths?(
          left_resource,
          candidate,
          [candidate_relationship.join_relationship | search],
          right_resource
        )

      true ->
        comparison_keys = [
          :source_field,
          :destination_field,
          :source_field_on_join_table,
          :destination_field_on_join_table,
          :destination_field,
          :destination
        ]

        Map.take(relationship, comparison_keys) ==
          Map.take(candidate_relationship, comparison_keys) and
          synonymous_relationship_paths?(relationship.destination, candidate_rest, rest)
    end
  end

  defp build_expr_with_predicate_information(expression) do
    expression = fully_simplify(expression)

    all_predicates =
      expression
      |> Filter.list_predicates()
      |> Enum.uniq()

    comparison_expressions =
      all_predicates
      |> Enum.filter(fn %module{} ->
        :erlang.function_exported(module, :compare, 2)
      end)
      |> Enum.reduce([], fn predicate, new_expressions ->
        all_predicates
        |> Enum.reject(&Kernel.==(&1, predicate))
        |> Enum.filter(&shares_ref?(&1, predicate))
        |> Enum.reduce(new_expressions, fn other_predicate, new_expressions ->
          # With predicate as a and other_predicate as b
          case Ash.Filter.Predicate.compare(predicate, other_predicate) do
            :right_includes_left ->
              # b || !a

              [b(other_predicate or not predicate) | new_expressions]

            :left_includes_right ->
              # a || ! b
              [b(predicate or not other_predicate) | new_expressions]

            :mutually_inclusive ->
              # (a && b) || (! a && ! b)
              [
                b((predicate and other_predicate) or (not predicate and not other_predicate))
                | new_expressions
              ]

            :mutually_exclusive ->
              [b(not (other_predicate and predicate)) | new_expressions]

            _other ->
              # If we can't tell, we assume that both could be true
              new_expressions
          end
        end)
      end)
      |> Enum.uniq()

    expression = filter_to_expr(expression)

    expression_with_comparisons =
      Enum.reduce(comparison_expressions, expression, fn comparison_expression, expression ->
        b(comparison_expression and expression)
      end)

    all_predicates
    |> Enum.map(& &1.__struct__)
    |> Enum.uniq()
    |> Enum.flat_map(fn struct ->
      if :erlang.function_exported(struct, :bulk_compare, 1) do
        struct.bulk_compare(all_predicates)
      else
        []
      end
    end)
    |> Enum.reduce(expression_with_comparisons, fn comparison_expression, expression ->
      b(comparison_expression and expression)
    end)
  end

  def fully_simplify(expression) do
    expression
    |> do_fully_simplify()
    |> lift_equals_out_of_in()
    |> do_fully_simplify()
  end

  defp do_fully_simplify(expression) do
    expression
    |> simplify()
    |> case do
      ^expression ->
        expression

      simplified ->
        fully_simplify(simplified)
    end
  end

  def lift_equals_out_of_in(expression) do
    case find_non_equal_overlap(expression) do
      nil ->
        expression

      non_equal_overlap ->
        expression
        |> split_in_expressions(non_equal_overlap)
        |> lift_equals_out_of_in()
    end
  end

  def find_non_equal_overlap(expression) do
    Ash.Filter.find(expression, fn sub_expr ->
      Ash.Filter.find(expression, fn sub_expr2 ->
        # if has_call_or_expression?(sub_expr) || has_call_or_expression?(sub_expr2) do
        #   false
        # else
        overlap?(sub_expr, sub_expr2)
        # end
      end)
    end)
  end

  defp new_in(base, right) do
    case MapSet.size(right) do
      1 ->
        %Ash.Query.Operator.Eq{left: base.left, right: Enum.at(right, 0)}

      _ ->
        %Ash.Query.Operator.In{left: base.left, right: right}
    end
  end

  def split_in_expressions(
        %Ash.Query.Operator.In{right: right} = sub_expr,
        %Ash.Query.Operator.Eq{right: value} = non_equal_overlap
      ) do
    if overlap?(non_equal_overlap, sub_expr) do
      Ash.Query.BooleanExpression.new(
        :or,
        new_in(sub_expr, MapSet.delete(right, value)),
        non_equal_overlap
      )
    else
      sub_expr
    end
  end

  def split_in_expressions(
        %Ash.Query.Operator.In{} = sub_expr,
        %Ash.Query.Operator.In{right: right} = non_equal_overlap
      ) do
    if overlap?(sub_expr, non_equal_overlap) do
      diff = MapSet.difference(sub_expr.right, right)

      if MapSet.size(diff) == 0 do
        Enum.reduce(sub_expr.right, nil, fn var, acc ->
          BooleanExpression.new(:or, %Ash.Query.Operator.Eq{left: sub_expr.left, right: var}, acc)
        end)
      else
        new_right = new_in(sub_expr, MapSet.intersection(sub_expr.right, right))

        Ash.Query.BooleanExpression.new(
          :or,
          new_in(sub_expr, diff),
          new_right
        )
      end
    else
      sub_expr
    end
  end

  def split_in_expressions(nil, _), do: nil

  def split_in_expressions(%Ash.Filter{expression: expression} = filter, non_equal_overlap),
    do: %{filter | expression: split_in_expressions(expression, non_equal_overlap)}

  def split_in_expressions(%Not{expression: expression} = not_expr, non_equal_overlap),
    do: %{not_expr | expression: split_in_expressions(expression, non_equal_overlap)}

  def split_in_expressions(
        %BooleanExpression{left: left, right: right} = expr,
        non_equal_overlap
      ),
      do: %{
        expr
        | left: split_in_expressions(left, non_equal_overlap),
          right: split_in_expressions(right, non_equal_overlap)
      }

  def split_in_expressions(other, _), do: other

  def overlap?(
        %Ash.Query.Operator.In{left: left, right: %{__struct__: MapSet} = left_right},
        %Ash.Query.Operator.In{left: left, right: %{__struct__: MapSet} = right_right}
      ) do
    if MapSet.equal?(left_right, right_right) do
      false
    else
      overlap? =
        left_right
        |> MapSet.intersection(right_right)
        |> MapSet.size()
        |> Kernel.>(0)

      if overlap? do
        true
      else
        false
      end
    end
  end

  def overlap?(_, %Ash.Query.Operator.Eq{right: %Ref{}}),
    do: false

  def overlap?(%Ash.Query.Operator.Eq{right: %Ref{}}, _),
    do: false

  def overlap?(
        %Ash.Query.Operator.Eq{left: left, right: left_right},
        %Ash.Query.Operator.In{left: left, right: %{__struct__: MapSet} = right_right}
      ) do
    MapSet.member?(right_right, left_right)
  end

  def overlap?(_left, _right) do
    false
  end

  def mutually_exclusive(predicates, acc \\ [])
  def mutually_exclusive([], acc), do: acc

  def mutually_exclusive([predicate | rest], acc) do
    new_acc =
      Enum.reduce(rest, acc, fn other_predicate, acc ->
        [b(not (predicate and other_predicate)) | acc]
      end)

    mutually_exclusive(rest, new_acc)
  end

  def left_excludes_right(left, right) do
    b(not (left and right))
  end

  def right_excludes_left(left, right) do
    b(not (right and left))
  end

  def mutually_inclusive(predicates, acc \\ [])
  def mutually_inclusive([], acc), do: acc

  def mutually_inclusive([predicate | rest], acc) do
    new_acc =
      Enum.reduce(rest, acc, fn other_predicate, acc ->
        [b((predicate and other_predicate) or (not predicate and not other_predicate)) | acc]
      end)

    mutually_exclusive(rest, new_acc)
  end

  def right_implies_left(left, right) do
    b(not (right and not left))
  end

  def left_implies_right(left, right) do
    b(not (left and not right))
  end

  defp shares_ref?(left, right) do
    any_refs_in_common?(refs(left), refs(right))
  end

  defp any_refs_in_common?(left_refs, right_refs) do
    Enum.any?(left_refs, &(&1 in right_refs))
  end

  defp refs(%{__operator__?: true, left: left, right: right}) do
    Enum.filter([left, right], &match?(%Ref{}, &1))
  end

  defp refs(%{__function__?: true, arguments: arguments}) do
    Enum.filter(arguments, &match?(%Ref{}, &1))
  end

  defp refs(_), do: []

  defp simplify(%BooleanExpression{op: op, left: left, right: right}) do
    BooleanExpression.new(op, simplify(left), simplify(right))
  end

  defp simplify(%Not{expression: expression}) do
    Not.new(simplify(expression))
  end

  defp simplify(%mod{__predicate__?: true} = predicate) do
    if :erlang.function_exported(mod, :simplify, 1) do
      predicate
      |> mod.simplify()
      |> Kernel.||(predicate)
    else
      predicate
    end
  end

  defp simplify(other), do: other

  def solve_expression(expression) do
    expression_with_constants = b(true and not false and expression)

    {bindings, expression} = extract_bindings(expression_with_constants)

    expression
    |> to_conjunctive_normal_form()
    |> lift_clauses()
    |> negations_to_negative_numbers()
    |> Enum.uniq()
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

    {bindings, b(not extracted)}
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
