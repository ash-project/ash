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

  @doc "Returns true if the value is or contains an expression"
  @spec expr?(term) :: boolean()
  def expr?({:_actor, _}), do: true
  def expr?({:_arg, _}), do: true
  def expr?({:_ref, _, _}), do: true
  def expr?({:_parent, _, _}), do: true
  def expr?({:_parent, _}), do: true
  def expr?({:_atomic_ref, _}), do: true
  def expr?({:_context, _}), do: true

  def expr?(value)
      when is_struct(value, Ash.Query.Not) or is_struct(value, Ash.Query.BooleanExpression) or
             is_struct(value, Ash.Query.Call) or is_struct(value, Ash.Query.Ref) or
             is_struct(value, Ash.Query.Exists) or
             is_struct(value, Ash.Query.Parent) or
             (is_struct(value) and is_map_key(value, :__predicate__?)) do
    true
  end

  def expr?(value) when is_list(value) do
    Enum.any?(value, &expr?/1)
  end

  def expr?(value) when is_map(value) and not is_struct(value) do
    Enum.any?(value, fn {key, value} ->
      expr?(key) || expr?(value)
    end)
  end

  def expr?({left, right}) do
    expr?(left) || expr?(right)
  end

  def expr?(tuple) when is_tuple(tuple) do
    tuple |> Tuple.to_list() |> expr?()
  end

  def expr?(_), do: false

  @doc "A template helper for using actor values in filter templates"
  def actor(value), do: {:_actor, value}

  @doc "A template helper for using action arguments in filter templates"
  def arg(name), do: {:_arg, name}

  @doc "A template helper for creating a reference"
  def ref(name), do: {:_ref, [], name}

  @doc "A template helper for creating a reference to a related path"
  def ref(path, name), do: {:_ref, path, name}

  @doc "A template helper for creating a parent reference"
  def parent(expr), do: {:_parent, [], expr}

  @doc "A template helper for referring to the most recent atomic expression applied to an update field"
  def atomic_ref(expr), do: {:_atomic_ref, expr}

  @doc """
  A template helper for using query context in filter templates

  An atom will get the value for a key, and a list will be accessed via `get_in`.
  """
  def context(name), do: {:_context, name}

  @doc false
  def eval_hydrated(expression, opts \\ []) do
    Ash.Filter.Runtime.load_and_eval(
      opts[:record],
      expression,
      opts[:parent],
      opts[:resource],
      opts[:domain],
      opts[:unknown_on_unknown_refs?]
    )
  end

  @spec where(Macro.t(), Macro.t()) :: t
  defmacro where(left, right) do
    quote do
      Ash.Query.BooleanExpression.optimized_new(
        :and,
        Ash.Expr.expr(unquote(left)),
        Ash.Expr.expr(unquote(right))
      )
    end
  end

  @spec or_where(Macro.t(), Macro.t()) :: t
  defmacro or_where(left, right) do
    quote do
      Ash.Query.BooleanExpression.optimized_new(
        :or,
        Ash.Expr.expr(unquote(left)),
        Ash.Expr.expr(unquote(right))
      )
    end
  end

  @spec expr(Macro.t()) :: t()
  defmacro expr(do: body) do
    quote location: :keep do
      Ash.Expr.expr(unquote(body))
    end
  end

  defmacro expr(body) do
    expr = do_expr(body)

    quote location: :keep do
      unquote(expr)
    end
  end

  @doc false
  def fill_template(
        template,
        actor \\ nil,
        args \\ %{},
        context \\ %{},
        changeset \\ nil
      ) do
    walk_template(template, fn
      {:_actor, :_primary_key} ->
        if actor do
          Map.take(actor, Ash.Resource.Info.primary_key(actor.__struct__))
        end

      {:_actor, field} when is_atom(field) or is_binary(field) ->
        Map.get(actor || %{}, field)

      {:_actor, path} when is_list(path) ->
        get_path(actor || %{}, path)

      {:_arg, field} ->
        case Map.fetch(args, field) do
          :error ->
            Map.get(args, to_string(field))

          {:ok, value} ->
            value
        end

      {:_atomic_ref, field} when is_atom(field) ->
        if changeset do
          Ash.Changeset.atomic_ref(changeset, field)
        else
          {:_atomic_ref, field}
        end

      {:_context, fields} when is_list(fields) ->
        get_path(context, fields)

      {:_context, field} ->
        Map.get(context, field)

      {:_ref, path, name} ->
        %Ash.Query.Ref{
          attribute: fill_template(name, actor, args, context),
          relationship_path: fill_template(path, actor, args, context)
        }

      %Ash.Query.Call{name: :sigil_i, args: [%Ash.Query.Call{name: :<<>>, args: [str]}, mods]} ->
        Ash.CiString.sigil_i(str, mods)

      other ->
        other
    end)
  end

  defp get_path(map, [key]) when is_struct(map) do
    Map.get(map, key)
  end

  defp get_path(map, [key]) when is_map(map) do
    Map.get(map, key)
  end

  defp get_path(map, [key | rest]) when is_map(map) do
    get_path(get_path(map, [key]), rest)
  end

  defp get_path(_, _), do: nil

  @doc false
  def template_references_actor?(template) do
    template_references?(template, fn
      {:_actor, _} -> true
      _ -> false
    end)
  end

  def template_references_argument?(template) do
    template_references?(template, fn
      {:_arg, _} -> true
      _ -> false
    end)
  end

  def template_references_context?(template) do
    template_references?(template, fn
      {:_context, _} -> true
      _ -> false
    end)
  end

  @doc "Whether or not a given template contains an actor reference"
  def template_references?(%BooleanExpression{op: :and, left: left, right: right}, pred) do
    template_references?(left, pred) || template_references?(right, pred)
  end

  def template_references?(%Not{expression: expression}, pred) do
    template_references?(expression, pred)
  end

  def template_references?(%Ash.Query.Exists{expr: expr}, pred) do
    template_references?(expr, pred)
  end

  def template_references?(%Ash.Query.Parent{expr: expr}, pred) do
    template_references?(expr, pred)
  end

  def template_references?(%{left: left, right: right}, pred) do
    template_references?(left, pred) || template_references?(right, pred)
  end

  def template_references?(%{arguments: args}, pred) do
    Enum.any?(args, &template_references?(&1, pred))
  end

  def template_references?(%Ash.Query.Call{args: args}, pred) do
    Enum.any?(args, &template_references?(&1, pred))
  end

  def template_references?(list, pred) when is_list(list) do
    Enum.any?(list, &template_references?(&1, pred))
  end

  def template_references?(map, pred) when is_map(map) and not is_struct(map) do
    Enum.any?(map, &template_references?(&1, pred))
  end

  def template_references?(tuple, pred) when is_tuple(tuple) do
    pred.(tuple) ||
      tuple
      |> Tuple.to_list()
      |> Enum.any?(&template_references?(&1, pred))
  end

  def template_references?(thing, pred), do: pred.(thing)

  @doc false
  def walk_template(filter, mapper) when is_list(filter) do
    case mapper.(filter) do
      ^filter ->
        Enum.map(filter, &walk_template(&1, mapper))

      other ->
        walk_template(other, mapper)
    end
  end

  def walk_template(%BooleanExpression{left: left, right: right} = expr, mapper) do
    case mapper.(expr) do
      ^expr ->
        %{
          expr
          | left: walk_template(left, mapper),
            right: walk_template(right, mapper)
        }

      other ->
        walk_template(other, mapper)
    end
  end

  def walk_template(%Not{expression: expression} = not_expr, mapper) do
    case mapper.(not_expr) do
      ^not_expr ->
        %{not_expr | expression: walk_template(expression, mapper)}

      other ->
        walk_template(other, mapper)
    end
  end

  def walk_template(%Ash.Query.Parent{expr: expr} = this_expr, mapper) do
    case mapper.(this_expr) do
      ^this_expr ->
        %{this_expr | expr: walk_template(expr, mapper)}

      other ->
        walk_template(other, mapper)
    end
  end

  def walk_template(%Ash.Query.Exists{expr: expr} = exists_expr, mapper) do
    case mapper.(exists_expr) do
      ^exists_expr ->
        %{exists_expr | expr: walk_template(expr, mapper)}

      other ->
        walk_template(other, mapper)
    end
  end

  def walk_template(%{__predicate__?: _, left: left, right: right} = pred, mapper) do
    case mapper.(pred) do
      ^pred ->
        %{
          pred
          | left: walk_template(left, mapper),
            right: walk_template(right, mapper)
        }

      other ->
        walk_template(other, mapper)
    end
  end

  def walk_template(%{__predicate__?: _, arguments: arguments} = func, mapper) do
    case mapper.(func) do
      ^func ->
        %{
          func
          | arguments: Enum.map(arguments, &walk_template(&1, mapper))
        }

      other ->
        walk_template(other, mapper)
    end
  end

  def walk_template(%Ash.Query.Call{args: args} = call, mapper) do
    case mapper.(call) do
      ^call ->
        %{
          call
          | args: Enum.map(args, &walk_template(&1, mapper))
        }

      other ->
        walk_template(other, mapper)
    end
  end

  def walk_template(filter, mapper) when is_map(filter) do
    if Map.has_key?(filter, :__struct__) do
      filter
    else
      case mapper.(filter) do
        ^filter ->
          Enum.into(filter, %{}, &walk_template(&1, mapper))

        other ->
          walk_template(other, mapper)
      end
    end
  end

  def walk_template(tuple, mapper) when is_tuple(tuple) do
    case mapper.(tuple) do
      ^tuple ->
        tuple
        |> Tuple.to_list()
        |> Enum.map(&walk_template(&1, mapper))
        |> List.to_tuple()

      other ->
        walk_template(other, mapper)
    end
  end

  def walk_template(value, mapper), do: mapper.(value)

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

    soft_escape(
      quote do
        [unquote(left), unquote(right)]
        |> Ash.Query.Function.GetPath.new()
        |> case do
          {:ok, call} ->
            call

          {:error, error} ->
            raise error
        end
      end,
      escape?
    )
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

  def do_expr({:ref, _, [field, path]} = expr, _escape?) do
    raise ArgumentError, """
    Usage of `ref/1` and `ref/2` must now be pinned, got: #{Macro.to_string(expr)}.

    For example: `^ref(#{Macro.to_string(remove_pin(field))}, #{Macro.to_string(remove_pin(path))})`
    """
  end

  def do_expr({:ref, _, [field]} = expr, _escape?) do
    raise ArgumentError, """
    Usage of `ref/1` and `ref/2` must now be pinned, got: #{Macro.to_string(expr)}.

    For example: `^ref(#{Macro.to_string(remove_pin(field))}})`
    """
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

  def do_expr({:lazy, _, args}, escape?) do
    soft_escape(%Ash.Query.Call{name: :lazy, args: args, operator?: false}, escape?)
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

  def do_expr({left, right}, escape?) do
    left = do_expr(left, escape?)
    right = do_expr(right, escape?)

    soft_escape({left, right}, escape?)
  end

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

  defp do_ref({left, _, nil}, _right) when left in @operator_symbols do
    raise ArgumentError, "invalid use of `.` in expression. Use `[]` to access nested fields"
  end

  defp do_ref({left, _, nil}, right) do
    %Ash.Query.Ref{relationship_path: [left], attribute: right}
  end

  defp do_ref({{:., _, [_, _]} = left, _, _}, right) do
    do_ref(left, right)
  end

  defp do_ref({:., _, [_left, _right]}, far_right) when far_right in @operator_symbols do
    raise ArgumentError, "invalid use of `.` in expression. Use `[]` to access nested fields"
  end

  defp do_ref({:., _, [left, right]}, far_right) do
    case do_ref(left, right) do
      %Ash.Query.Ref{relationship_path: path, attribute: attribute} = ref ->
        %{ref | relationship_path: path ++ [attribute], attribute: far_right}

      :error ->
        :error
    end
  end

  defp do_ref({left, _, _}, right) when left in @operator_symbols and is_atom(right) do
    raise ArgumentError, "invalid use of `.` in expression. Use `[]` to access nested fields"
  end

  defp do_ref({left, _, _}, right) when is_atom(left) and right in @operator_symbols do
    raise ArgumentError, "invalid use of `.` in expression. Use `[]` to access nested fields"
  end

  defp do_ref({left, _, _}, right) when is_atom(left) and is_atom(right) do
    %Ash.Query.Ref{relationship_path: [left], attribute: right}
  end

  defp do_ref(_left, _right) do
    :error
  end

  defp remove_pin({:^, _, [value]}), do: value
  defp remove_pin(value), do: value
end
