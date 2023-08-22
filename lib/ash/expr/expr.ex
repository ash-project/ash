defmodule Ash.Expr do
  @moduledoc "Tools to build Ash expressions"
  alias Ash.Query.{BooleanExpression, Not}

  @type t :: any
  @pass_through_funcs [:where, :or_where, :expr, :@]

  @doc """
  Evaluate an expression. See `eval/2` for more.
  """
  def eval!(expression, opts \\ []) do
    case eval(expression, opts) do
      {:ok, result} ->
        result

      {:error, error} ->
        raise Ash.Error.to_ash_error(error)
    end
  end

  @doc """
  Evaluate an expression. This function only works if you have no references, or if you provide the `record` option.
  """
  def eval(expression, opts \\ []) do
    context =
      opts[:context]
      |> Kernel.||(%{})
      |> Map.put_new(:resource, opts[:resource])

    expression
    |> Ash.Filter.hydrate_refs(context)
    |> case do
      {:ok, hydrated} ->
        eval_hydrated(hydrated, opts)

      {:error, error} ->
        {:error, error}
    end
  end

  @doc false
  def eval_hydrated(expression, opts \\ []) do
    Ash.Filter.Runtime.load_and_eval(
      opts[:record],
      expression,
      opts[:parent],
      opts[:resource],
      opts[:api]
    )
  end

  defmacro where(left, right) do
    quote do
      Ash.Query.BooleanExpression.optimized_new(
        :and,
        Ash.Expr.expr(unquote(left)),
        Ash.Expr.expr(unquote(right))
      )
    end
  end

  defmacro or_where(left, right) do
    quote do
      Ash.Query.BooleanExpression.optimized_new(
        :or,
        Ash.Expr.expr(unquote(left)),
        Ash.Expr.expr(unquote(right))
      )
    end
  end

  defmacro expr(do: body) do
    quote location: :keep do
      Ash.Expr.expr(unquote(body))
    end
  end

  defmacro expr(body) do
    if Keyword.keyword?(body) do
      quote location: :keep do
        unquote(body)
      end
    else
      expr = do_expr(body)

      quote location: :keep do
        unquote(expr)
      end
    end
  end

  @operator_symbols Ash.Query.Operator.operator_symbols() -- [:is_nil]

  @doc false
  def do_expr(expr, escape? \\ true)

  def do_expr({:|>, _, [first, {func, meta, args}]}, escape?) do
    do_expr({func, meta, [first | args]}, escape?)
  end

  def do_expr({func, _, _} = expr, _escape?) when func in @pass_through_funcs do
    expr
  end

  def do_expr({{:., _, [_, func]}, _, _} = expr, _escape?)
      when func in @pass_through_funcs do
    expr
  end

  def do_expr({op, _, nil}, escape?) when is_atom(op) do
    soft_escape(%Ash.Query.Ref{relationship_path: [], attribute: op}, escape?)
  end

  def do_expr({op, _, Elixir}, escape?) when is_atom(op) do
    soft_escape(%Ash.Query.Ref{relationship_path: [], attribute: op}, escape?)
  end

  def do_expr({:__aliases__, _, _} = expr, _escape?) do
    expr
  end

  def do_expr({:^, _, [value]}, _escape?) do
    value
  end

  def do_expr({{:., _, [Access, :get]}, _, [left, right]}, escape?) do
    left = do_expr(left, false)
    right = do_expr(right, false)

    [left, right]
    |> Ash.Query.Function.GetPath.new()
    |> case do
      {:ok, call} ->
        soft_escape(call, escape?)

      {:error, error} ->
        raise error
    end
  end

  def do_expr({{:., _, [_, _]} = left, _, []}, escape?) do
    do_expr(left, escape?)
  end

  def do_expr(value, escape?) when is_list(value) do
    Enum.map(value, &do_expr(&1, escape?))
  end

  def do_expr({:%{}, _, keys}, escape?) do
    {:%{}, [],
     Enum.map(keys, fn {key, value} -> {do_expr(key, escape?), do_expr(value, escape?)} end)}
  end

  # def do_expr({{:., _, [at_path, agg]}, _, [path, expr]}, escape?)
  #     when agg in @inline_aggregates do
  #   expr_with_at_path(path, at_path, expr, Ash.Query.InlineAggregate, escape?)
  # end

  def do_expr({{:., _, [at_path, :exists]}, _, [path, expr]}, escape?) do
    expr_with_at_path(path, at_path, expr, Ash.Query.Exists, escape?)
  end

  def do_expr({{:., _, [_, _]} = left, _, args}, escape?) do
    args = Enum.map(args, &do_expr(&1, false))

    case do_expr(left, escape?) do
      {:%{}, [], parts} = other when is_list(parts) ->
        if Enum.any?(parts, &(&1 == {:__struct__, Ash.Query.Ref})) do
          ref = Map.new(parts)

          soft_escape(
            %Ash.Query.Call{
              name: ref.attribute,
              relationship_path: ref.relationship_path,
              args: args,
              operator?: false
            },
            escape?
          )
        else
          other
        end

      %Ash.Query.Ref{} = ref ->
        soft_escape(
          %Ash.Query.Call{
            name: ref.attribute,
            relationship_path: ref.relationship_path,
            args: args,
            operator?: false
          },
          escape?
        )

      other ->
        other
    end
  end

  def do_expr({:ref, _, [field, path]}, escape?) do
    ref =
      case do_expr(path, false) do
        %Ash.Query.Ref{attribute: head_attr, relationship_path: head_path} ->
          case do_expr(field) do
            %Ash.Query.Ref{attribute: tail_attribute, relationship_path: tail_relationship_path} ->
              %Ash.Query.Ref{
                relationship_path: head_path ++ [head_attr] ++ tail_relationship_path,
                attribute: tail_attribute
              }

            other ->
              %Ash.Query.Ref{relationship_path: head_path ++ [head_attr], attribute: other}
          end

        other ->
          case do_expr(field, false) do
            %Ash.Query.Ref{attribute: attribute, relationship_path: relationship_path} ->
              %Ash.Query.Ref{
                attribute: attribute,
                relationship_path: List.wrap(other) ++ List.wrap(relationship_path)
              }

            other_field ->
              %Ash.Query.Ref{attribute: other_field, relationship_path: other}
          end
      end

    soft_escape(ref, escape?)
  end

  def do_expr({:ref, _, [field]}, escape?) do
    ref =
      case do_expr(field, false) do
        %Ash.Query.Ref{} = ref ->
          ref

        other ->
          %Ash.Query.Ref{attribute: other, relationship_path: []}
      end

    soft_escape(ref, escape?)
  end

  def do_expr({:., _, [left, right]} = ref, escape?) when is_atom(right) do
    case do_ref(left, right) do
      %Ash.Query.Ref{} = ref ->
        soft_escape(ref, escape?)

      :error ->
        raise "Invalid reference! #{Macro.to_string(ref)}"
    end
  end

  def do_expr({op, _, args}, escape?) when op in [:and, :or] do
    args = Enum.map(args, &do_expr(&1, false))

    soft_escape(BooleanExpression.optimized_new(op, Enum.at(args, 0), Enum.at(args, 1)), escape?)
  end

  def do_expr({op, _, [_, _] = args}, escape?)
      when is_atom(op) and op in @operator_symbols do
    args = Enum.map(args, &do_expr(&1, false))

    soft_escape(%Ash.Query.Call{name: op, args: args, operator?: true}, escape?)
  end

  def do_expr({parent, _, [expr]}, escape?) when parent in [:parent, :source, :parent_expr] do
    expr = do_expr(expr, escape?)

    soft_escape(
      quote do
        Ash.Query.Parent.new(unquote(expr))
      end,
      escape?
    )
  end

  def do_expr({:exists, _, [path, original_expr]}, escape?) do
    expr_with_at_path(path, [], original_expr, Ash.Query.Exists, escape?)
  end

  def do_expr({left, _, [{op, _, [right]}]}, escape?)
      when is_atom(op) and op in @operator_symbols and is_atom(left) and left != :not do
    args = Enum.map([{left, [], nil}, right], &do_expr(&1, false))

    soft_escape(%Ash.Query.Call{name: op, args: args, operator?: true}, escape?)
  end

  def do_expr({:not, _, [expression]}, escape?) do
    expression = do_expr(expression, false)

    soft_escape(Not.new(expression), escape?)
  end

  def do_expr({:cond, _, [[do: options]]}, escape?) do
    options
    |> Enum.map(fn {:->, _, [condition, result]} ->
      {condition, result}
    end)
    |> cond_to_if_tree()
    |> do_expr(escape?)
  end

  def do_expr({:fragment, _, [first | _]}, _escape?) when not is_binary(first) do
    raise "to prevent SQL injection attacks, fragment(...) does not allow strings " <>
            "to be interpolated as the first argument via the `^` operator, got: `#{inspect(first)}`"
  end

  def do_expr({op, _, args}, escape?) when is_atom(op) and is_list(args) do
    last_arg = List.last(args)

    args =
      if Keyword.keyword?(last_arg) && Keyword.has_key?(last_arg, :do) do
        Enum.map(:lists.droplast(args), &do_expr(&1, false)) ++
          [
            Enum.map(last_arg, fn {key, arg_value} ->
              {key, do_expr(arg_value, false)}
            end)
          ]
      else
        Enum.map(args, &do_expr(&1, false))
      end

    soft_escape(%Ash.Query.Call{name: op, args: args, operator?: false}, escape?)
  end

  def do_expr({left, _, _}, escape?) when is_tuple(left), do: do_expr(left, escape?)

  def do_expr(other, _), do: other

  defp expr_with_at_path(path, at_path, expr, struct, escape?) do
    expr = do_expr(expr, escape?)

    path =
      case path do
        {:^, _, [value]} ->
          value

        {:., _, [left, right]} ->
          ref = do_ref(left, right)
          ref.relationship_path ++ [ref.attribute]

        {{:., _, [left, right]}, _, _} ->
          ref = do_ref(left, right)
          ref.relationship_path ++ [ref.attribute]

        {atom, _, _} when is_atom(atom) ->
          [atom]

        path when is_list(path) ->
          path

        other ->
          raise "Invalid value used in the first argument in exists, i.e exists(#{Macro.to_string(other)}, #{Macro.to_string(expr)})"
      end

    at_path =
      case at_path do
        {:^, _, [value]} ->
          value

        {:., _, [left, right]} ->
          ref = do_ref(left, right)
          ref.relationship_path ++ [ref.attribute]

        {{:., _, [left, right]}, _, _} ->
          ref = do_ref(left, right)
          ref.relationship_path ++ [ref.attribute]

        {atom, _, _} when is_atom(atom) ->
          [atom]

        path when is_list(path) ->
          path

        other ->
          raise "Invalid value used in the first argument in exists, i.e exists(#{Macro.to_string(other)}, #{Macro.to_string(at_path)})"
      end

    soft_escape(
      quote do
        unquote(struct).new(
          unquote(path),
          unquote(expr),
          unquote(at_path)
        )
      end,
      escape?
    )
  end

  defp cond_to_if_tree([{condition, result}]) do
    {:if, [], [cond_condition(condition), [do: result]]}
  end

  defp cond_to_if_tree([{condition, result} | rest]) do
    {:if, [], [cond_condition(condition), [do: result, else: cond_to_if_tree(rest)]]}
  end

  defp cond_condition([condition]) do
    condition
  end

  defp cond_condition([condition | rest]) do
    {:and, [], [condition, cond_condition(rest)]}
  end

  defp soft_escape(%_{} = val, _) do
    {:%{}, [], Map.to_list(val)}
  end

  defp soft_escape(other, _), do: other

  defp do_ref({left, _, nil}, right) do
    %Ash.Query.Ref{relationship_path: [left], attribute: right}
  end

  defp do_ref({{:., _, [_, _]} = left, _, _}, right) do
    do_ref(left, right)
  end

  defp do_ref({:., _, [left, right]}, far_right) do
    case do_ref(left, right) do
      %Ash.Query.Ref{relationship_path: path, attribute: attribute} = ref ->
        %{ref | relationship_path: path ++ [attribute], attribute: far_right}

      :error ->
        :error
    end
  end

  defp do_ref({left, _, _}, right) when is_atom(left) and is_atom(right) do
    %Ash.Query.Ref{relationship_path: [left], attribute: right}
  end

  defp do_ref(_left, _right) do
    :error
  end
end
