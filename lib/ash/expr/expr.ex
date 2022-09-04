defmodule Ash.Expr do
  @moduledoc "Tools to build Ash expressions"
  alias Ash.Query.{BooleanExpression, Not}

  @type t :: Macro.t()

  defmacro expr(do: body) do
    quote do
      Ash.Expr.expr(unquote(body))
    end
  end

  defmacro expr(body) do
    if Keyword.keyword?(body) do
      quote do
        unquote(body)
      end
    else
      expr = do_expr(body)

      quote do
        unquote(expr)
      end
    end
  end

  @operator_symbols Ash.Query.Operator.operator_symbols()

  @doc false
  def do_expr(expr, escape? \\ true)

  def do_expr({op, _, nil}, escape?) when is_atom(op) do
    soft_escape(%Ash.Query.Ref{relationship_path: [], attribute: op}, escape?)
  end

  def do_expr({op, _, Elixir}, escape?) when is_atom(op) do
    soft_escape(%Ash.Query.Ref{relationship_path: [], attribute: op}, escape?)
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
