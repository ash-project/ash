defmodule Ash.SatSolver do
  @moduledoc """
  Tools for working with the satsolver that drives filter subset checking (for authorization)
  """

  alias Ash.Filter
  alias Ash.Query.{BooleanExpression, Not, Ref}

  @dialyzer {:nowarn_function, overlap?: 2}

  @doc """
  Creates tuples of a boolean statement.

  i.e `b(1 and 2) #=> {:and, 1, 2}`
  """
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

  @doc false
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

  @doc "Returns true if the candidate filter returns the same or less data than the filter"
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
    filter =
      Filter.map(filter, fn
        %Ref{} = ref ->
          %{ref | input?: false}

        other ->
          other
      end)

    candidate =
      Filter.map(candidate, fn
        %Ref{} = ref ->
          %{ref | input?: false}

        other ->
          other
      end)

    expr = BooleanExpression.new(:and, filter.expression, candidate.expression)

    case transform_and_solve(
           filter.resource,
           expr
         ) do
      {:error, :unsatisfiable} ->
        false

      {:ok, _scenario} ->
        expr = BooleanExpression.new(:and, Not.new(filter.expression), candidate.expression)

        case transform_and_solve(
               filter.resource,
               expr
             ) do
          {:error, :unsatisfiable} ->
            true

          {:ok, _scenario} ->
            :maybe
        end
    end
  end

  defp filter_to_expr(nil), do: nil
  defp filter_to_expr(false), do: false
  defp filter_to_expr(true), do: true
  defp filter_to_expr(%Filter{expression: expression}), do: filter_to_expr(expression)
  defp filter_to_expr(%{__predicate__?: _} = op_or_func), do: op_or_func
  defp filter_to_expr(%Ash.Query.Exists{} = exists), do: exists
  defp filter_to_expr(%Ash.Query.Parent{} = parent), do: parent
  defp filter_to_expr(%Ash.CustomExpression{expression: expression}), do: expression
  defp filter_to_expr(%Not{expression: expression}), do: b(not filter_to_expr(expression))

  defp filter_to_expr(%BooleanExpression{op: op, left: left, right: right}) do
    {op, filter_to_expr(left), filter_to_expr(right)}
  end

  defp filter_to_expr(expr) do
    raise ArgumentError, message: "Invalid filter expression #{inspect(expr)}"
  end

  @doc "Prepares a filter for comparison"
  def transform(resource, expression) do
    expression
    |> consolidate_relationships(resource)
    |> upgrade_related_filters_to_join_keys(resource)
    |> build_expr_with_predicate_information()
  end

  @doc "Calls `transform/2` and solves the expression"
  def transform_and_solve(resource, expression) do
    resource
    |> transform(expression)
    |> to_cnf()
    |> elem(0)
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
         %Ash.Query.Exists{path: path, expr: expr} = exists,
         resource
       ) do
    related = Ash.Resource.Info.related(resource, path)

    %{exists | expr: upgrade_related_filters_to_join_keys(expr, related)}
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

  defp upgrade_ref({key, ref}, resource) when is_atom(key) do
    {key, upgrade_ref(ref, resource)}
  end

  defp upgrade_ref(
         %Ash.Query.Ref{attribute: attribute, relationship_path: path} = ref,
         resource
       )
       when path != [] do
    with relationship when not is_nil(relationship) <-
           Ash.Resource.Info.relationship(resource, path),
         true <- attribute.name == relationship.destination_attribute,
         new_attribute when not is_nil(new_attribute) <-
           Ash.Resource.Info.attribute(relationship.source, relationship.source_attribute) do
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
      |> Filter.relationship_paths(true)
      |> Enum.uniq()
      |> Enum.reduce({%{}, []}, fn path, {replacements, kept_paths} ->
        case find_synonymous_relationship_path(resource, kept_paths, path) do
          nil ->
            {replacements, [path | kept_paths]}

          synonymous_path ->
            Map.put(replacements, path, synonymous_path)
        end
      end)

    do_consolidate_relationships(expression, replacements, resource)
  end

  defp do_consolidate_relationships(
         %BooleanExpression{op: op, left: left, right: right},
         replacements,
         resource
       ) do
    BooleanExpression.new(
      op,
      do_consolidate_relationships(left, replacements, resource),
      do_consolidate_relationships(right, replacements, resource)
    )
  end

  defp do_consolidate_relationships(%Not{expression: expression}, replacements, resource) do
    Not.new(do_consolidate_relationships(expression, replacements, resource))
  end

  defp do_consolidate_relationships(
         %Ash.Query.Exists{at_path: at_path, path: path, expr: expr} = exists,
         replacements,
         resource
       ) do
    exists =
      case Map.fetch(replacements, at_path) do
        {:ok, replacement} when not is_nil(replacement) ->
          %{exists | at_path: replacement}

        :error ->
          exists
      end

    related = Ash.Resource.Info.related(resource, at_path)

    {replacements, _all_relationship_paths} =
      expr
      |> Filter.relationship_paths(true)
      |> Enum.uniq()
      |> Enum.reduce({%{}, []}, fn path, {replacements, kept_paths} ->
        case find_synonymous_relationship_path(related, kept_paths, path) do
          nil ->
            {replacements, [path | kept_paths]}

          synonymous_path ->
            Map.put(replacements, path, synonymous_path)
        end
      end)

    exists =
      case Map.fetch(replacements, path) do
        {:ok, replacement} when not is_nil(replacement) ->
          %{exists | path: replacement}

        :error ->
          exists
      end

    full_related = Ash.Resource.Info.related(related, path)

    %{exists | expr: consolidate_relationships(expr, full_related)}
  end

  defp do_consolidate_relationships(
         %Ash.Query.Ref{relationship_path: path} = ref,
         replacements,
         _resource
       )
       when path != [] do
    case Map.fetch(replacements, path) do
      {:ok, replacement} when not is_nil(replacement) -> %{ref | relationship_path: replacement}
      :error -> ref
    end
  end

  defp do_consolidate_relationships(
         %{__function__?: true, arguments: args} = func,
         replacements,
         resource
       ) do
    %{func | arguments: Enum.map(args, &do_consolidate_relationships(&1, replacements, resource))}
  end

  defp do_consolidate_relationships(
         %{__operator__?: true, left: left, right: right} = op,
         replacements,
         resource
       ) do
    %{
      op
      | left: do_consolidate_relationships(left, replacements, resource),
        right: do_consolidate_relationships(right, replacements, resource)
    }
  end

  defp do_consolidate_relationships(other, _, _), do: other

  defp find_synonymous_relationship_path(resource, paths, path) do
    Enum.find_value(paths, fn candidate_path ->
      if synonymous_relationship_paths?(resource, candidate_path, path) do
        candidate_path
      else
        false
      end
    end)
  end

  @doc "Returns `true` if the relationship paths are synonymous from a data perspective"
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
        [candidate_first | candidate_rest],
        [first | rest],
        right_resource
      ) do
    right_resource = right_resource || left_resource
    relationship = Ash.Resource.Info.relationship(left_resource, first)
    candidate_relationship = Ash.Resource.Info.relationship(right_resource, candidate_first)

    cond do
      !relationship || !candidate_relationship ->
        false

      relationship.type == :many_to_many && candidate_relationship.type == :has_many ->
        synonymous_relationship_paths?(left_resource, [relationship.join_relationship], [
          candidate_first
        ]) && !Enum.empty?(candidate_rest) &&
          synonymous_relationship_paths?(
            left_resource,
            candidate_rest,
            rest,
            right_resource
          )

      relationship.type == :has_many && candidate_relationship.type == :many_to_many ->
        synonymous_relationship_paths?(left_resource, [relationship.name], [
          candidate_relationship.join_relationship
        ]) && !Enum.empty?(rest) &&
          synonymous_relationship_paths?(
            left_resource,
            candidate_rest,
            rest,
            right_resource
          )

      true ->
        comparison_keys = [
          :source_attribute,
          :destination_attribute,
          :source_attribute_on_join_resource,
          :destination_attribute_on_join_resource,
          :destination_attribute,
          :destination,
          :manual,
          :sort,
          :filter
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

            :mutually_exclusive_and_collectively_exhaustive ->
              [
                b(
                  not (other_predicate and predicate) and
                    not (not other_predicate and not predicate)
                )
                | new_expressions
              ]

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

  defp fully_simplify(expression) do
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

  defp overlap?(
         %Ash.Query.Operator.In{left: left, right: %MapSet{} = left_right},
         %Ash.Query.Operator.In{left: left, right: %MapSet{} = right_right}
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

  defp overlap?(_, %Ash.Query.Operator.Eq{right: %Ref{}}),
    do: false

  defp overlap?(%Ash.Query.Operator.Eq{right: %Ref{}}, _),
    do: false

  defp overlap?(
         %Ash.Query.Operator.Eq{left: left, right: left_right},
         %Ash.Query.Operator.In{left: left, right: %MapSet{} = right_right}
       ) do
    MapSet.member?(right_right, left_right)
  end

  defp overlap?(_left, _right) do
    false
  end

  @doc "Returns a statement expressing that the predicates are mutually exclusive."
  def mutually_exclusive(predicates, acc \\ [])
  def mutually_exclusive([], acc), do: acc

  def mutually_exclusive([predicate | rest], acc) do
    new_acc =
      Enum.reduce(rest, acc, fn other_predicate, acc ->
        [b(not (predicate and other_predicate)) | acc]
      end)

    mutually_exclusive(rest, new_acc)
  end

  @doc "Returns a statement expressing that the predicates are mutually exclusive and collectively exhaustive."
  def mutually_exclusive_and_collectively_exhaustive([]), do: []

  def mutually_exclusive_and_collectively_exhaustive([_]), do: []

  def mutually_exclusive_and_collectively_exhaustive(predicates) do
    mutually_exclusive(predicates) ++
      Enum.flat_map(predicates, fn predicate ->
        other_predicates = Enum.reject(predicates, &(&1 == predicate))

        other_predicates_union =
          Enum.reduce(other_predicates, nil, fn other_predicate, expr ->
            if expr do
              b(expr or other_predicate)
            else
              other_predicate
            end
          end)

        b(
          not (predicate and other_predicates_union) and
            not (not predicate and not other_predicates_union)
        )
      end)
  end

  @doc "Returns `b(not (left and right))`"
  def left_excludes_right(left, right) do
    b(not (left and right))
  end

  @doc "Returns `b(not (right and left))`"
  def right_excludes_left(left, right) do
    b(not (right and left))
  end

  @doc "Returns a statement expressing that the predicates are mutually inclusive"
  def mutually_inclusive(predicates, acc \\ [])
  def mutually_inclusive([], acc), do: acc

  def mutually_inclusive([predicate | rest], acc) do
    new_acc =
      Enum.reduce(rest, acc, fn other_predicate, acc ->
        [b((predicate and other_predicate) or (not predicate and not other_predicate)) | acc]
      end)

    mutually_exclusive(rest, new_acc)
  end

  @doc "Returns `b(not (right and not left))`"
  def right_implies_left(left, right) do
    b(not (right and not left))
  end

  @doc "Returns `b(not (left and not right))`"
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
    Enum.filter([left, right], &match?(%Ref{}, &1)) |> Enum.map(&Map.put(&1, :input?, false))
  end

  defp refs(%{__function__?: true, arguments: arguments}) do
    Enum.filter(arguments, &match?(%Ref{}, &1)) |> Enum.map(&Map.put(&1, :input?, false))
  end

  defp refs(_), do: []

  defp simplify(%BooleanExpression{op: op, left: left, right: right}) do
    BooleanExpression.new(op, simplify(left), simplify(right))
  end

  defp simplify(%Not{expression: expression}) do
    Not.new(simplify(expression))
  end

  defp simplify(%Ash.Query.Exists{expr: expr} = exists) do
    %{exists | expr: simplify(expr)}
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

  @doc """
  Transforms a statement to Conjunctive Normal Form(CNF), as lists of lists of integers.
  """
  def to_cnf(expression) do
    expression_with_constants = b(true and not false and expression)

    {bindings, expression} = extract_bindings(expression_with_constants)

    expression
    |> to_conjunctive_normal_form()
    |> lift_clauses()
    |> negations_to_negative_numbers()
    |> Enum.map(fn scenario ->
      Enum.sort_by(scenario, fn item ->
        {abs(item), item}
      end)
    end)
    |> group_predicates(bindings)
    |> rebind()
    |> unique_clauses()
  end

  defp unique_clauses({clauses, bindings}) do
    {Enum.uniq(clauses), bindings}
  end

  defp group_predicates(expression, bindings) do
    case expression do
      [_] ->
        {expression, bindings}

      scenarios ->
        Enum.reduce(scenarios, {[], bindings}, fn scenario, {new_scenarios, bindings} ->
          {scenario, bindings} = group_scenario_predicates(scenario, scenarios, bindings)
          {[scenario | new_scenarios], bindings}
        end)
    end
  end

  defp group_scenario_predicates(scenario, all_scenarios, bindings) do
    scenario
    |> Ash.SatSolver.Utils.ordered_sublists()
    |> Enum.filter(&can_be_used_as_group?(&1, all_scenarios, bindings))
    |> Enum.sort_by(&length/1)
    |> remove_overlapping()
    |> Enum.reduce({scenario, bindings}, fn group, {scenario, bindings} ->
      bindings = add_group_binding(bindings, group)

      {Ash.SatSolver.Utils.replace_ordered_sublist(scenario, group, bindings[:groups][group]),
       bindings}
    end)
  end

  defp remove_overlapping([]), do: []

  defp remove_overlapping([item | rest]) do
    if Enum.any?(item, fn n ->
         Enum.any?(rest, &(n in &1 or -n in &1))
       end) do
      remove_overlapping(rest)
    else
      [item | remove_overlapping(rest)]
    end
  end

  @doc "Remaps integers back to clauses"
  def unbind(expression, %{temp_bindings: temp_bindings, old_bindings: old_bindings}) do
    expression =
      Enum.flat_map(expression, fn statement ->
        Enum.flat_map(statement, fn var ->
          neg? = var < 0
          old_binding = temp_bindings[abs(var)]

          case old_bindings[:reverse_groups][old_binding] do
            nil ->
              if neg? do
                [-old_binding]
              else
                [old_binding]
              end

            group ->
              if neg? do
                Enum.map(group, &(-&1))
              else
                [{:expand, group}]
              end
          end
        end)
        |> expand_groups()
      end)

    {expression, old_bindings}
  end

  defp expand_groups(expression) do
    do_expand_groups(expression)
  end

  defp do_expand_groups([]), do: [[]]

  defp do_expand_groups([{:expand, group} | rest]) do
    Enum.flat_map(group, fn var ->
      Enum.map(do_expand_groups(rest), fn future ->
        [var | future]
      end)
    end)
  end

  defp do_expand_groups([var | rest]) do
    Enum.map(do_expand_groups(rest), fn future ->
      [var | future]
    end)
  end

  defp rebind({expression, bindings}) do
    {expression, temp_bindings} =
      Enum.reduce(expression, {[], %{current: 0}}, fn statement, {statements, acc} ->
        {statement, acc} =
          Enum.reduce(statement, {[], acc}, fn var, {statement, acc} ->
            case acc[:reverse][abs(var)] do
              nil ->
                binding = acc.current + 1

                value =
                  if var < 0 do
                    -binding
                  else
                    binding
                  end

                {[value | statement],
                 acc
                 |> Map.put(:current, binding)
                 |> Map.update(:reverse, %{abs(var) => binding}, &Map.put(&1, abs(var), binding))
                 |> Map.put(binding, abs(var))}

              value ->
                value =
                  if var < 0 do
                    -value
                  else
                    value
                  end

                {[value | statement], acc}
            end
          end)

        {[Enum.reverse(statement) | statements], acc}
      end)

    bindings_with_old_bindings = %{temp_bindings: temp_bindings, old_bindings: bindings}

    {expression, bindings_with_old_bindings}
  end

  defp can_be_used_as_group?(group, scenarios, bindings) do
    Map.has_key?(bindings[:groups] || %{}, group) ||
      Enum.all?(scenarios, fn scenario ->
        has_no_overlap?(scenario, group) || group_in_scenario?(scenario, group)
      end)
  end

  defp has_no_overlap?(scenario, group) do
    not Enum.any?(group, fn group_predicate ->
      Enum.any?(scenario, fn scenario_predicate ->
        abs(group_predicate) == abs(scenario_predicate)
      end)
    end)
  end

  defp group_in_scenario?(scenario, group) do
    Ash.SatSolver.Utils.is_ordered_sublist_of?(group, scenario)
  end

  defp add_group_binding(bindings, group) do
    if bindings[:groups][group] do
      bindings
    else
      binding = bindings[:current]

      bindings
      |> Map.put_new(:reverse_groups, %{})
      |> Map.update!(:reverse_groups, &Map.put(&1, binding, group))
      |> Map.put_new(:groups, %{})
      |> Map.update!(:groups, &Map.put(&1, group, binding))
      |> Map.put(:current, binding + 1)
    end
  end

  def solve_expression(cnf) do
    Ash.SatSolver.Implementation.solve_expression(cnf)
  end

  def contains?([], _), do: false

  def contains?([_ | t] = l1, l2) do
    List.starts_with?(l1, l2) or contains?(t, l2)
  end

  def solutions_to_predicate_values(solution, bindings) do
    Enum.reduce(solution, %{true: [], false: []}, fn var, state ->
      fact = Map.get(bindings, abs(var))

      if is_nil(fact) do
        raise Ash.Error.Framework.AssumptionFailed.exception(
                message: """
                A fact from the sat solver had no corresponding bound fact:

                Bindings:
                  #{inspect(bindings)}

                Missing:
                  #{inspect(var)}
                """
              )
      end

      Map.put(state, fact, var > 0)
    end)
  end

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
