defmodule Ash.Flow.Template do
  @moduledoc """
  Helpers for working with Ash.Flow templates.

  At first glance it would seem that all of this could be replaced by some calls to
  `Macro.prewalk` but that is unfortunately not the case. We don't traverse through structs,
  and in some cases we do things like return ranges/merge maps that require having the entire
  nested part of the template, i.e `{:_merge, [list, of, maps]}` has to get the fully handled
  list of maps, and so has to call itself.
  """

  def is_template?(value) when is_map(value) do
    Enum.any?(value, fn {key, value} ->
      is_template?(key) || is_template?(value)
    end)
  end

  def is_template?(value) when is_list(value) do
    Enum.any?(value, &is_template?/1)
  end

  def is_template?({:_path, _, _}), do: true
  def is_template?({:_merge, _}), do: true

  def is_template?({:_expr, expr}) do
    not is_nil(Ash.Filter.find(expr, &is_template?/1))
  end

  def is_template?({:_result, _}), do: true
  def is_template?({:_element, _}), do: true

  def is_template?(tuple) when is_tuple(tuple) do
    tuple
    |> Tuple.to_list()
    |> Enum.any?(&is_template?/1)
  end

  def is_template?(_), do: false

  def remap_result_references(action_input, prefix) do
    do_remap_result_references(action_input, prefix)
  end

  defp do_remap_result_references(action_input, prefix)
       when is_map(action_input) and not is_struct(action_input) do
    Map.new(action_input, fn {key, value} ->
      new_key = do_remap_result_references(key, prefix)
      new_val = do_remap_result_references(value, prefix)
      {new_key, new_val}
    end)
  end

  defp do_remap_result_references(action_input, prefix) when is_list(action_input) do
    Enum.map(action_input, &do_remap_result_references(&1, prefix))
  end

  defp do_remap_result_references({:_path, value, path}, prefix) do
    {:_path, do_remap_result_references(value, prefix), do_remap_result_references(path, prefix)}
  end

  defp do_remap_result_references({:_merge, items}, prefix) do
    {:_merge, do_remap_result_references(items, prefix)}
  end

  defp do_remap_result_references({:_expr, expr}, prefix) do
    {:_expr, Ash.Expr.walk_template(expr, &do_remap_result_references(&1, prefix))}
  end

  defp do_remap_result_references({:_result, step}, prefix) when is_function(prefix) do
    {:_result, prefix.(step)}
  end

  defp do_remap_result_references({:_result, step}, prefix) do
    {:_result, [prefix | List.wrap(step)]}
  end

  defp do_remap_result_references({:_element, step}, prefix) when is_function(prefix) do
    {:_element, prefix.(step)}
  end

  defp do_remap_result_references({:_element, step}, prefix) do
    {:_element, [prefix | List.wrap(step)]}
  end

  defp do_remap_result_references(action_input, input) when is_tuple(action_input) do
    List.to_tuple(do_remap_result_references(Tuple.to_list(action_input), input))
  end

  defp do_remap_result_references(other, _), do: other

  def set_dependent_values(action_input, input) do
    do_set_dependent_values(action_input, input)
  end

  defp do_set_dependent_values(action_input, input)
       when is_map(action_input) and not is_struct(action_input) do
    Map.new(action_input, fn {key, value} ->
      new_key = do_set_dependent_values(key, input)
      new_val = do_set_dependent_values(value, input)
      {new_key, new_val}
    end)
  end

  defp do_set_dependent_values(action_input, input) when is_list(action_input) do
    Enum.map(action_input, &do_set_dependent_values(&1, input))
  end

  defp do_set_dependent_values({:_path, value, path}, input) do
    {:_path, do_set_dependent_values(value, input), do_set_dependent_values(path, input)}
  end

  defp do_set_dependent_values({:_expr, expr}, input) do
    {:_expr, Ash.Expr.walk_template(expr, &do_set_dependent_values(&1, input))}
  end

  defp do_set_dependent_values({:_merge, items}, input) do
    items
    |> Enum.map(&do_set_dependent_values(&1, input))
    |> Enum.reject(&is_nil/1)
    |> Enum.reduce(%{}, &Map.merge(&2, &1))
  end

  defp do_set_dependent_values({:_result, step}, input) do
    get_in(input, [:results, step])
  end

  defp do_set_dependent_values({:_element, step}, input) do
    get_in(input, [:elements, step])
  end

  defp do_set_dependent_values({:_range, start, finish}, input) do
    do_set_dependent_values(start, input)..do_set_dependent_values(finish, input)
  end

  defp do_set_dependent_values(action_input, input) when is_tuple(action_input) do
    List.to_tuple(do_set_dependent_values(Tuple.to_list(action_input), input))
  end

  defp do_set_dependent_values(other, _), do: other

  def arg_refs(input) when is_map(input) do
    Enum.flat_map(input, &arg_refs/1)
  end

  def arg_refs(input) when is_list(input) do
    Enum.flat_map(input, &arg_refs/1)
  end

  def arg_refs({:_expr, expr}) do
    list_expr_refs(
      expr,
      fn
        {:_arg, _} ->
          true

        _ ->
          false
      end
    )
    |> Enum.map(&elem(&1, 1))
  end

  def arg_refs({:_arg, name}) do
    [name]
  end

  def arg_refs(input) when is_tuple(input) do
    input
    |> Tuple.to_list()
    |> Enum.flat_map(&arg_refs/1)
  end

  def arg_refs(_), do: []

  def element_refs(input) when is_map(input) do
    Enum.flat_map(input, &element_refs/1)
  end

  def element_refs(input) when is_list(input) do
    Enum.flat_map(input, &element_refs/1)
  end

  def element_refs({:_expr, expr}) do
    list_expr_refs(
      expr,
      fn
        {:_element, _} ->
          true

        _ ->
          false
      end
    )
    |> Enum.map(&elem(&1, 1))
  end

  def element_refs({:_element, name}) do
    [name]
  end

  def element_refs(input) when is_tuple(input) do
    input
    |> Tuple.to_list()
    |> Enum.flat_map(&element_refs/1)
  end

  def element_refs(_), do: []

  def result_refs(input) when is_map(input) do
    Enum.flat_map(input, &result_refs/1)
  end

  def result_refs(input) when is_list(input) do
    Enum.flat_map(input, &result_refs/1)
  end

  def result_refs({:_expr, expr}) do
    list_expr_refs(
      expr,
      fn
        {:_result, _} ->
          true

        _ ->
          false
      end
    )
    |> Enum.map(&elem(&1, 1))
  end

  def result_refs({:_result, name}) do
    [name]
  end

  def result_refs(input) when is_tuple(input) do
    input
    |> Tuple.to_list()
    |> Enum.flat_map(&result_refs/1)
  end

  def result_refs(_), do: []

  defp list_expr_refs(expression, matcher) do
    expression
    |> do_list_expr_refs(matcher)
    |> Enum.uniq()
  end

  defp do_list_expr_refs(list, matcher) when is_list(list) do
    Enum.flat_map(list, &do_list_expr_refs(&1, matcher))
  end

  defp do_list_expr_refs({key, value}, matcher) when is_atom(key) do
    if matcher.({key, value}) do
      [{key, value}]
    else
      do_list_expr_refs(value, matcher)
    end
  end

  defp do_list_expr_refs(expression, matcher) do
    case expression do
      %Ash.Query.BooleanExpression{left: left, right: right} ->
        do_list_expr_refs(left, matcher) ++ do_list_expr_refs(right, matcher)

      %Ash.Query.Not{expression: not_expr} ->
        do_list_expr_refs(not_expr, matcher)

      %{__predicate__?: _, left: left, right: right} ->
        do_list_expr_refs(left, matcher) ++
          do_list_expr_refs(right, matcher)

      %{__predicate__?: _, arguments: args} ->
        Enum.flat_map(args, &do_list_expr_refs(&1, matcher))

      %Ash.Query.Call{args: args} ->
        args
        |> Enum.flat_map(&do_list_expr_refs(&1, matcher))

      v ->
        if matcher.(v) do
          [v]
        else
          []
        end
    end
  end

  def handle_input_template(action_input, input) do
    {val, deps} = do_handle_input_template(action_input, input)
    {val, Enum.uniq(deps)}
  end

  defp do_handle_input_template({:_expr, expr}, input) do
    {new_expr, deps} = do_handle_input_template(expr, input)

    {{:_expr, new_expr}, deps}
  end

  defp do_handle_input_template(
         %Ash.Query.BooleanExpression{left: left, right: right} = expr,
         input
       ) do
    {new_left, left_deps} = do_handle_input_template(left, input)
    {new_right, right_deps} = do_handle_input_template(right, input)
    {%{expr | left: new_left, right: new_right}, left_deps ++ right_deps}
  end

  defp do_handle_input_template(%Ash.Query.Not{expression: expression} = not_expr, input) do
    {new_expr, deps} = do_handle_input_template(expression, input)
    {%{not_expr | expression: new_expr}, deps}
  end

  defp do_handle_input_template(%{__predicate__?: _, left: left, right: right} = pred, input) do
    {new_left, left_deps} = do_handle_input_template(left, input)
    {new_right, right_deps} = do_handle_input_template(right, input)
    {%{pred | left: new_left, right: new_right}, left_deps ++ right_deps}
  end

  defp do_handle_input_template(%{__predicate__?: _, arguments: arguments} = func, input) do
    {args, deps} =
      Enum.reduce(arguments, {[], []}, fn arg, {args, deps} ->
        {new_arg, new_deps} = do_handle_input_template(arg, input)
        {[new_arg | args], deps ++ new_deps}
      end)

    {%{func | arguments: Enum.reverse(args)}, deps}
  end

  defp do_handle_input_template(%Ash.Query.Call{args: arguments} = call, input) do
    {args, deps} =
      Enum.reduce(arguments, {[], []}, fn arg, {args, deps} ->
        {new_arg, new_deps} = do_handle_input_template(arg, input)
        {[new_arg | args], deps ++ new_deps}
      end)

    {%{call | args: Enum.reverse(args)}, deps}
  end

  defp do_handle_input_template(action_input, input)
       when is_map(action_input) and not is_struct(action_input) do
    Enum.reduce(action_input, {%{}, []}, fn {key, value}, {acc, deps} ->
      {new_key, key_deps} = do_handle_input_template(key, input)
      {new_val, val_deps} = do_handle_input_template(value, input)
      {Map.put(acc, new_key, new_val), deps ++ key_deps ++ val_deps}
    end)
  end

  defp do_handle_input_template(action_input, input) when is_list(action_input) do
    {new_items, deps} =
      Enum.reduce(action_input, {[], []}, fn item, {items, deps} ->
        {new_item, new_deps} = do_handle_input_template(item, input)

        {[new_item | items], new_deps ++ deps}
      end)

    {Enum.reverse(new_items), deps}
  end

  defp do_handle_input_template({:_path, value, path}, input) do
    {new_value, value_deps} = do_handle_input_template(value, input)
    {new_path, path_deps} = do_handle_input_template(path, input)
    {{:_path, new_value, new_path}, value_deps ++ path_deps}
  end

  defp do_handle_input_template({:_merge, items}, input) do
    {new_items, deps} =
      Enum.reduce(items, {[], []}, fn item, {items, deps} ->
        {new_item, new_deps} = do_handle_input_template(item, input)

        {[new_item | items], new_deps ++ deps}
      end)

    {{:_merge, new_items}, deps}
  end

  defp do_handle_input_template({:_arg, name}, input) do
    case Map.fetch(input, name) do
      {:ok, value} ->
        {value, []}

      :error ->
        {Map.get(input, to_string(name)), []}
    end
  end

  defp do_handle_input_template({:_result, step}, _input) do
    {{:_result, step}, [{:_result, step}]}
  end

  defp do_handle_input_template(action_input, input) when is_tuple(action_input) do
    {list, deps} = do_handle_input_template(Tuple.to_list(action_input), input)
    {List.to_tuple(list), deps}
  end

  defp do_handle_input_template(other, _), do: {other, []}
end
