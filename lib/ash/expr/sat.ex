# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Expr.SAT do
  @moduledoc false

  alias Ash.Filter
  alias Ash.Query.{BooleanExpression, Not, Ref}
  require Crux.Expression

  @dialyzer {:nowarn_function, overlap?: 2}

  @doc "Prepares a filter for comparison"
  @spec to_sat_expression(Ash.Resource.t(), Ash.Expr.t()) ::
          Crux.Expression.t(Ash.Expr.t())
  def to_sat_expression(resource, expression) do
    expression
    |> consolidate_relationships(resource)
    |> upgrade_related_filters_to_join_keys(resource)
    |> build_expr_with_predicate_information()
  end

  defp filter_to_expr(nil), do: nil
  defp filter_to_expr(false), do: false
  defp filter_to_expr(true), do: true
  defp filter_to_expr(%Filter{expression: expression}), do: filter_to_expr(expression)
  defp filter_to_expr(%{__predicate__?: _} = op_or_func), do: op_or_func
  defp filter_to_expr(%Ash.Query.Exists{} = exists), do: exists
  defp filter_to_expr(%Ash.Query.Parent{} = parent), do: parent
  defp filter_to_expr(%Ash.CustomExpression{expression: expression}), do: expression

  defp filter_to_expr(%Not{expression: expression}),
    do: Crux.Expression.b(not filter_to_expr(expression))

  defp filter_to_expr(%BooleanExpression{op: op, left: left, right: right}) do
    {op, filter_to_expr(left), filter_to_expr(right)}
  end

  defp filter_to_expr(expr) do
    raise ArgumentError, message: "Invalid filter expression #{inspect(expr)}"
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
      if Ash.Resource.Info.synonymous_relationship_paths?(resource, candidate_path, path) do
        candidate_path
      else
        false
      end
    end)
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

              [Crux.Expression.b(other_predicate or not predicate) | new_expressions]

            :left_includes_right ->
              # a || ! b
              [Crux.Expression.b(predicate or not other_predicate) | new_expressions]

            :mutually_inclusive ->
              # (a && b) || (! a && ! b)
              [
                Crux.Expression.b(
                  (predicate and other_predicate) or (not predicate and not other_predicate)
                )
                | new_expressions
              ]

            :mutually_exclusive ->
              [Crux.Expression.b(nand(other_predicate, predicate)) | new_expressions]

            :mutually_exclusive_and_collectively_exhaustive ->
              [
                Crux.Expression.b(
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
        Crux.Expression.b(comparison_expression and expression)
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
      Crux.Expression.b(comparison_expression and expression)
    end)
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

  defp lift_equals_out_of_in(expression) do
    case find_non_equal_overlap(expression) do
      nil ->
        expression

      non_equal_overlap ->
        expression
        |> split_in_expressions(non_equal_overlap)
        |> lift_equals_out_of_in()
    end
  end

  defp find_non_equal_overlap(expression) do
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

  defp split_in_expressions(
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

  defp split_in_expressions(
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

  defp split_in_expressions(nil, _), do: nil

  defp split_in_expressions(%Ash.Filter{expression: expression} = filter, non_equal_overlap),
    do: %{filter | expression: split_in_expressions(expression, non_equal_overlap)}

  defp split_in_expressions(%Not{expression: expression} = not_expr, non_equal_overlap),
    do: %{not_expr | expression: split_in_expressions(expression, non_equal_overlap)}

  defp split_in_expressions(
         %BooleanExpression{left: left, right: right} = expr,
         non_equal_overlap
       ),
       do: %{
         expr
         | left: split_in_expressions(left, non_equal_overlap),
           right: split_in_expressions(right, non_equal_overlap)
       }

  defp split_in_expressions(other, _), do: other

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
end
