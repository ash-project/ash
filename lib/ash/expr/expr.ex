# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Expr do
  @moduledoc "Tools to build Ash expressions"
  alias Ash.Query.{BooleanExpression, Not}

  @doc "Prepares a filter for comparison"
  defdelegate to_sat_expression(resource, expression), to: Ash.Expr.SAT

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
      |> Map.put(:eval?, true)

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
  def expr?({:_combinations, _}), do: true
  def expr?({:_parent, _, _}), do: true
  def expr?({:_parent, _}), do: true
  def expr?({:_atomic_ref, _}), do: true
  def expr?({:_context, _}), do: true

  def expr?(value)
      when is_struct(value, Ash.Query.Not) or is_struct(value, Ash.Query.BooleanExpression) or
             is_struct(value, Ash.Query.Call) or is_struct(value, Ash.Query.Ref) or
             is_struct(value, Ash.Query.Exists) or
             is_struct(value, Ash.Query.Parent) or
             is_struct(value, Ash.Query.UpsertConflict) or
             is_struct(value, Ash.CustomExpression) or
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

  @doc "A template helper for using the tenant in filter templates"
  def tenant, do: :_tenant

  @doc "A template helper for using action arguments in filter templates"
  def arg(name), do: {:_arg, name}

  @doc "A template helper for creating a reference"
  def ref(name) when is_atom(name), do: {:_ref, [], name}

  @doc "A template helper for creating a reference to a related path"
  def ref(path, name) when is_list(path) and is_atom(name), do: {:_ref, path, name}

  @doc "A template helper for creating a reference"
  def combinations(name) when is_atom(name), do: {:_combinations, name}

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
      opts[:unknown_on_unknown_refs?],
      opts[:actor],
      opts[:tenant]
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

  @doc """
  Creates an expression calculation for use in sort and distinct statements.

  ## Examples

  ```elixir
  Ash.Query.sort(query, [
    {calc(string_upcase(name), :asc},
    {calc(count_nils([field1, field2]), type: :integer), :desc})
  ])
  ```
  """
  @spec calc(Macro.t(), opts :: Keyword.t()) :: t()
  defmacro calc(expression, opts \\ []) do
    quote generated: true do
      require Ash.Expr
      opts = unquote(opts)
      type = opts[:type] && Ash.Type.get_type(opts[:type])
      constraints = opts[:constraints] || []
      name = opts[:name] || :__calc__

      case Ash.Query.Calculation.new(
             name,
             Ash.Resource.Calculation.Expression,
             [expr: Ash.Expr.expr(unquote(expression))],
             type,
             constraints
           ) do
        {:ok, calc} -> calc
        {:error, term} -> raise Ash.Error.to_ash_error(term)
      end
    end
  end

  @doc """
  Creates an expression. See the [Expressions guide](/documentation/topics/reference/expressions.md) for more.
  """
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
        opts
      )
      when is_list(opts) do
    walk_template(template, fn
      {:_actor, :_primary_key} ->
        actor = opts[:actor]

        if actor do
          Map.take(actor, Ash.Resource.Info.primary_key(actor.__struct__))
        end

      {:_actor, field} when is_atom(field) or is_binary(field) ->
        Map.get(opts[:actor] || %{}, field)

      {:_actor, path} when is_list(path) ->
        get_path(opts[:actor] || %{}, path)

      :_tenant ->
        opts[:tenant]

      {:_arg, field} ->
        args = opts[:args]

        case Map.fetch(args, field) do
          :error ->
            Map.get(args, to_string(field))

          {:ok, value} ->
            value
        end

      {:_atomic_ref, field} when is_atom(field) ->
        changeset = opts[:changeset]

        if changeset do
          Ash.Changeset.atomic_ref(changeset, field)
        else
          {:_atomic_ref, field}
        end

      {:_context, fields} when is_list(fields) ->
        get_path(opts[:context], fields)

      {:_context, field} ->
        Map.get(opts[:context], field)

      {:_ref, path, name} ->
        %Ash.Query.Ref{
          attribute: fill_template(name, Keyword.take(opts, [:actor, :tenant, :args, :context])),
          relationship_path:
            fill_template(path, Keyword.take(opts, [:actor, :tenant, :args, :context]))
        }

      {:_combinations, name} ->
        %Ash.Query.Ref{
          attribute: fill_template(name, Keyword.take(opts, [:actor, :tenant, :args, :context])),
          combinations?: true
        }

      other ->
        other
    end)
  end

  @doc false
  def fill_template(
        template,
        actor \\ nil,
        args \\ %{},
        context \\ %{},
        changeset \\ nil
      ) do
    fill_template(template,
      actor: actor,
      args: args,
      context: context,
      changeset: changeset
    )
  end

  @doc false
  def get_path(map, [key]) when is_map(map) do
    Map.get(map, key)
  end

  def get_path(map, [key | rest]) when is_map(map) do
    get_path(get_path(map, [key]), rest)
  end

  def get_path(_, _), do: nil

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

  def can_return_nil?(nil), do: true

  def can_return_nil?(%Ash.Query.BooleanExpression{left: left, right: right}) do
    can_return_nil?(left) || can_return_nil?(right)
  end

  def can_return_nil?(%Ash.Query.Not{expression: expression}) do
    can_return_nil?(expression)
  end

  def can_return_nil?(%Ash.Query.Parent{expr: expr}) do
    can_return_nil?(expr)
  end

  def can_return_nil?(%Ash.Query.UpsertConflict{}), do: true

  def can_return_nil?(%Ash.Query.Exists{}), do: false

  def can_return_nil?(%mod{__predicate__?: _} = pred) do
    mod.can_return_nil?(pred)
  end

  def can_return_nil?(%Ash.Query.Ref{attribute: %{allow_nil?: false}}), do: false

  def can_return_nil?(value) do
    if Ash.Expr.expr?(value) do
      true
    else
      false
    end
  end

  @doc "Whether or not a given template contains an actor reference"
  def template_references?(%{__struct__: Ash.Filter, expression: expression}, pred) do
    template_references?(expression, pred)
  end

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

  def template_references?(
        %Ash.CustomExpression{expression: expression, simple_expression: simple_expression},
        pred
      ) do
    template_references?(expression, pred) || template_references?(simple_expression, pred)
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

  def walk_template(%{__struct__: Ash.Filter, expression: expression} = filter, mapper) do
    %{filter | expression: walk_template(expression, mapper)}
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

  def do_expr({:{}, _, vals}, escape?) do
    {%{}, [], Enum.map(vals, fn value -> do_expr(value, escape?) end)}
  end

  def do_expr({{:., _, [at_path, :exists]}, _, [path, expr]}, escape?) do
    expr_with_at_path(path, at_path, expr, Ash.Query.Exists, escape?)
  end

  def do_expr({{:., _, [at_path, :exists]}, _, [path]}, escape?) do
    expr_with_at_path(path, at_path, true, Ash.Query.Exists, escape?)
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

  def do_expr(
        {:<<>>, meta,
         [
           {:"::", _meta1,
            [{{:., _meta2, [Kernel, :to_string]}, _meta3, [left]}, {:binary, _, _}]}
         ]},
        escape?
      ) do
    do_expr({:type, meta, [left, :string]}, escape?)
  end

  def do_expr(
        {:<<>>, meta, [second_to_last, last]},
        escape?
      ) do
    do_expr({:<>, meta, [second_to_last, last]}, escape?)
  end

  def do_expr({:<<>>, _meta, [single]}, _escape?) do
    single
  end

  def do_expr(
        {:<<>>, meta, [next | rest]},
        escape?
      ) do
    do_expr({:<>, meta, [next, {:<<>>, meta, rest}]}, escape?)
  end

  def do_expr(
        {:"::", meta, [{{:., _meta1, [Kernel, :to_string]}, _meta2, [left]}, {:binary, _, _}]},
        escape?
      ) do
    do_expr({:type, meta, [left, :string]}, escape?)
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

    if op in [:==, :!=, :>, :<, :>=, :<=] do
      soft_escape(
        quote do
          args = unquote(args)

          call = %Ash.Query.Call{name: unquote(op), args: args, operator?: true}

          if Enum.any?(args, &is_nil/1) do
            IO.warn(
              "Comparing values with `nil` will always return `false`. Use `is_nil/1` instead. In: `#{inspect(call)}`"
            )
          end

          call
        end,
        escape?
      )
    else
      soft_escape(%Ash.Query.Call{name: op, args: args, operator?: true}, escape?)
    end
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

  def do_expr({:upsert_conflict, _, [expr]}, escape?) do
    expr = do_expr(expr, escape?)

    soft_escape(
      quote do
        Ash.Query.UpsertConflict.new(unquote(expr))
      end,
      escape?
    )
  end

  def do_expr({:exists, _, [{:__aliases__, _, _parts} = alias_ast, original_expr]}, escape?) do
    processed_expr = do_expr(original_expr, false)

    soft_escape(
      quote do
        %Ash.Query.Exists{
          path: [],
          resource: Macro.escape(unquote(alias_ast)),
          expr: unquote(processed_expr),
          at_path: [],
          related?: false
        }
      end,
      escape?
    )
  end

  def do_expr({:exists, _, [{:__aliases__, _, _parts} = alias_ast]}, escape?) do
    soft_escape(
      quote do
        %Ash.Query.Exists{
          path: [],
          resource: Macro.escape(unquote(alias_ast)),
          expr: true,
          at_path: [],
          related?: false
        }
      end,
      escape?
    )
  end

  def do_expr({:exists, _, [module_atom, original_expr]}, escape?) when is_atom(module_atom) do
    module_string = Atom.to_string(module_atom)

    if String.match?(module_string, ~r/^[A-Z].*/) do
      processed_expr = do_expr(original_expr, false)

      soft_escape(
        quote do
          %Ash.Query.Exists{
            path: [],
            resource: unquote(module_atom),
            expr: unquote(processed_expr),
            at_path: [],
            related?: false
          }
        end,
        escape?
      )
    else
      expr_with_at_path(module_atom, [], original_expr, Ash.Query.Exists, escape?)
    end
  end

  def do_expr({:exists, _, [module_atom]}, escape?) when is_atom(module_atom) do
    module_string = Atom.to_string(module_atom)

    if String.match?(module_string, ~r/^[A-Z].*/) do
      soft_escape(
        %Ash.Query.Exists{
          path: [],
          resource: module_atom,
          expr: true,
          at_path: [],
          related?: false
        },
        escape?
      )
    else
      expr_with_at_path(module_atom, [], true, Ash.Query.Exists, escape?)
    end
  end

  def do_expr({:exists, _, [path, original_expr]}, escape?) do
    expr_with_at_path(path, [], original_expr, Ash.Query.Exists, escape?)
  end

  def do_expr({:exists, _, [path]}, escape?) do
    expr_with_at_path(path, [], true, Ash.Query.Exists, escape?)
  end

  def do_expr({left, _, [{op, _, [right]}]}, escape?)
      when is_atom(op) and op in @operator_symbols and is_atom(left) and left != :not do
    do_expr({op, [], [left, right]}, escape?)
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

  def do_expr({:case, _, _}, _escape?) do
    raise ArgumentError,
      message: """
      `case` expressions are not supported in Ash expressions.
      Please use `cond` expressions instead. For example:

      # Instead of:
      case role do
        :principal -> 1
        :teacher -> 2
        :student -> 3
      end

      # Use:
      cond do
        role == :principal -> 1
        role == :teacher -> 2
        role == :student -> 3
      end
      """
  end

  def do_expr({:lazy, _, args}, escape?) do
    soft_escape(%Ash.Query.Call{name: :lazy, args: args, operator?: false}, escape?)
  end

  def do_expr({:sigil_i, _, [{:<<>>, _, [str]}, mods]}, escape?) do
    soft_escape(Ash.CiString.sigil_i(str, mods), escape?)
  end

  def do_expr({:fragment, _, [{_, _, [{:<<>>, _, [query]}, []]} = first | args]}, escape?)
      when is_binary(query) do
    args = Enum.map(args, &do_expr(&1, false))

    soft_escape(%Ash.Query.Call{name: :fragment, args: [first | args], operator?: false}, escape?)
  end

  def do_expr({:fragment, _, [first | _] = args}, escape?)
      when is_binary(first) or is_function(first) do
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

    soft_escape(%Ash.Query.Call{name: :fragment, args: args, operator?: false}, escape?)
  end

  def do_expr(
        {:&, _,
         [
           {:/, _,
            [
              {{:., _, [{:__aliases__, _, [_]}, _]}, _, []},
              _
            ]}
         ]} = expr,
        _
      ) do
    expr
  end

  def do_expr(
        {:&, _,
         [
           {:/, _,
            [
              {{:., _, [v, _]}, _, []},
              _
            ]}
         ]} = expr,
        _
      )
      when is_atom(v) do
    expr
  end

  def do_expr(
        {:&, _,
         [
           {:/, _,
            [
              {{:., _, [{mod, _, context}, _]}, _, []},
              _
            ]}
         ]} = expr,
        _
      )
      when is_atom(mod) and is_atom(context) do
    expr
  end

  def do_expr(
        {:&, _, _} = expr,
        _
      ) do
    raise """
    The only kind of anonymous functions allowed in expressions are in the format `&Module.function/arity`.

    Got: #{Macro.to_string(expr)}
    """
  end

  def do_expr(
        {:fn, _, _} = expr,
        _
      ) do
    raise """
    The only kind of anonymous functions allowed in expressions are in the format `&Module.function/arity`.

    Got: #{Macro.to_string(expr)}
    """
  end

  def do_expr({:fragment, _, [{:&, _, _} | _] = args}, escape?) do
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

    soft_escape(%Ash.Query.Call{name: :fragment, args: args, operator?: false}, escape?)
  end

  def do_expr({:fragment, _, [{m, f, a} | _] = args}, escape?)
      when is_atom(m) and is_atom(f) and is_list(a) do
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

    soft_escape(%Ash.Query.Call{name: :fragment, args: args, operator?: false}, escape?)
  end

  def do_expr({:fragment, _, [first | _]}, _escape?) do
    raise """
    To prevent SQL injection attacks, fragment(...) allows only two specific kinds of values

    1. A string literal *not* interpolated. This is for use with data layers like `AshPostgres.
    2. A one argument function or an MFA *not* interpolated. This is for use with data layers like `Ash.DataLayer.Simple` and `Ash.DataLayer.Ets`.

    Got: #{Macro.to_string(first)}
    """
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

  def determine_types(mod, args, returns \\ nil, nested? \\ false)

  def determine_types(Ash.Query.Function.Type, [_, type], _returns, _nested?) do
    {type, []}
  end

  def determine_types(Ash.Query.Function.Type, [_, type, constraints], _returns, _nested?) do
    {type, constraints}
  end

  def determine_types(mod, values, known_result, _nested?) do
    Code.ensure_compiled(mod)

    known_result =
      case known_result do
        nil -> nil
        {:array, type} -> {{:array, type}, []}
        {type, constraints} -> {type, constraints}
        type -> {type, []}
      end

    name =
      cond do
        function_exported?(mod, :operator, 0) ->
          mod.operator()

        function_exported?(mod, :name, 0) ->
          mod.name()

        true ->
          nil
      end

    cond do
      :erlang.function_exported(mod, :types, 0) ->
        {mod.types(), mod.returns()}

      :erlang.function_exported(mod, :args, 0) ->
        {mod.args(), mod.returns()}

      true ->
        {[:any], [:any]}
    end
    |> then(fn {types, returns} ->
      if types == :var_args || returns == :no_return || returns == :unknown do
        []
      else
        overloads = Ash.Query.Operator.operator_overloads(name) || %{}

        more_types = Map.keys(overloads)
        more_returns = Map.values(overloads)
        types = Enum.concat(types, List.wrap(more_types))
        returns = Enum.concat(returns, List.wrap(more_returns))

        returns =
          Enum.map(returns, fn
            {:array, any} when any in [:same, :any] -> {:array, any}
            any when any in [:same, :any] -> any
            {type, constraints} -> get_type({type, constraints})
            type -> get_type({type, []})
          end)

        types =
          Enum.map(types, fn
            types when is_list(types) ->
              Enum.map(types, fn
                {:array, any} when any in [:same, :any] -> {:array, any}
                any when any in [:same, :any] -> any
                {type, constraints} -> get_type({type, constraints})
                type -> get_type({type, []})
              end)

            types ->
              types
          end)

        Enum.zip(types, returns)
      end
    end)
    |> Enum.reject(fn {typeset, _} -> typeset == :any end)
    |> Enum.filter(fn {typeset, _} ->
      typeset == :same ||
        length(typeset) == length(values)
    end)
    |> Enum.map(fn {typeset, returns} ->
      basis =
        cond do
          !returns ->
            nil

          returns == :same ->
            known_result

          returns == {:array, :same} ->
            case known_result do
              {:array, type} ->
                case type do
                  {type, constraints} ->
                    {type, constraints}

                  type ->
                    {type, []}
                end

              _ ->
                nil
            end

          true ->
            nil
        end

      types_and_values =
        if typeset == :same do
          Enum.map(values, &{:same, &1})
        else
          Enum.zip(typeset, values)
        end

      types_and_values
      |> Enum.with_index()
      |> Enum.reduce_while(
        %{must_adopt_basis: [], basis: basis, types: [], fallback_basis: nil},
        fn
          {{vague_type, value}, index}, acc when vague_type in [:any, :same] ->
            case determine_type(value) do
              {:ok, {type, constraints}} ->
                case acc[:basis] do
                  nil ->
                    if vague_type == :any do
                      acc = Map.update!(acc, :types, &[{type, constraints} | &1])
                      {:cont, Map.put(acc, :basis, {type, constraints})}
                    else
                      acc =
                        acc
                        |> Map.update!(:types, &[nil | &1])
                        |> Map.put(:fallback_basis, {type, constraints})

                      {:cont, Map.update!(acc, :must_adopt_basis, &[{index, fn x -> x end} | &1])}
                    end

                  {^type, matched_constraints} ->
                    {:cont, Map.update!(acc, :types, &[{type, matched_constraints} | &1])}

                  _basis ->
                    {:halt, :error}
                end

              :error ->
                acc = Map.update!(acc, :types, &[nil | &1])
                {:cont, Map.update!(acc, :must_adopt_basis, &[{index, fn x -> x end} | &1])}
            end

          {{{:array, vague_type}, value}, index}, acc when vague_type in [:any, :same] ->
            case determine_type(value) do
              {:ok, {{:array, type}, constraints}} ->
                case acc[:basis] do
                  nil ->
                    if vague_type == :any do
                      acc = Map.update!(acc, :types, &[{:array, {type, constraints}} | &1])
                      {:cont, Map.put(acc, :basis, {type, constraints})}
                    else
                      acc =
                        acc
                        |> Map.update!(:types, &[nil | &1])
                        |> Map.put(:fallback_basis, {type, constraints})

                      {:cont,
                       Map.update!(
                         acc,
                         :must_adopt_basis,
                         &[
                           {index,
                            fn {type, constraints} -> {{:array, type}, items: constraints} end}
                           | &1
                         ]
                       )}
                    end

                  {^type, matched_constraints} ->
                    {:cont,
                     Map.update!(acc, :types, &[{:array, {type, matched_constraints}} | &1])}

                  _ ->
                    {:halt, :error}
                end

              _ ->
                acc = Map.update!(acc, :types, &[nil | &1])

                {:cont,
                 Map.update!(
                   acc,
                   :must_adopt_basis,
                   &[
                     {index, fn {type, constraints} -> {{:array, type}, items: constraints} end}
                     | &1
                   ]
                 )}
            end

          {{{type, constraints}, value}, _index}, acc ->
            determined_type = determine_type(value)

            cond do
              !Ash.Expr.expr?(value) && !matches_type?(type, value, constraints) ->
                case Ash.Type.coerce(type, value, constraints) do
                  {:ok, _} ->
                    {:cont, Map.update!(acc, :types, &[{type, constraints} | &1])}

                  _ ->
                    {:halt, :error}
                end

              match?({:ok, {determined_type, _}} when determined_type != type, determined_type) ->
                {:halt, :error}

              match?({:ok, _}, determined_type) ->
                {:cont, Map.update!(acc, :types, &[elem(determined_type, 1) | &1])}

              Ash.Expr.expr?(value) ->
                {:cont, Map.update!(acc, :types, &[{type, constraints} | &1])}

              true ->
                {:cont, Map.update!(acc, :types, &[{type, constraints} | &1])}
            end

          {{type, value}, _index}, acc ->
            determined_type = determine_type(value)

            cond do
              !Ash.Expr.expr?(value) && !matches_type?(type, value, []) ->
                case Ash.Type.coerce(type, value, []) do
                  {:ok, _} ->
                    {:cont, Map.update!(acc, :types, &[{type, []} | &1])}

                  _ ->
                    {:halt, :error}
                end

              match?({:ok, {determined_type, _}} when determined_type != type, determined_type) ->
                {:halt, :error}

              match?({:ok, _}, determined_type) ->
                {:cont, Map.update!(acc, :types, &[elem(determined_type, 1) | &1])}

              Ash.Expr.expr?(value) ->
                {:cont, Map.update!(acc, :types, &[{type, []} | &1])}

              true ->
                {:cont, Map.update!(acc, :types, &[{type, []} | &1])}
            end
        end
      )
      |> then(fn
        %{basis: nil, fallback_basis: fallback_basis} = data when not is_nil(fallback_basis) ->
          %{data | basis: fallback_basis}

        data ->
          data
      end)
      |> case do
        :error ->
          nil

        %{basis: nil, must_adopt_basis: [], types: types} ->
          if returns not in [:same, :any, {:array, :same}, {:array, :any}] do
            {Enum.reverse(types), returns, Enum.count(types)}
          end

        %{basis: nil, must_adopt_basis: _} ->
          nil

        %{basis: basis, must_adopt_basis: basis_adopters, types: types} ->
          returns =
            case returns do
              same when same in [:same, :any] ->
                basis

              same when same in [{:array, :same}, {:array, :any}] ->
                {type, constraints} = basis
                {{:array, type}, items: constraints}

              other ->
                other
            end

          {basis_adopters
           |> Enum.reduce(
             Enum.reverse(types),
             fn {index, function_of_basis}, types ->
               List.replace_at(types, index, function_of_basis.(basis))
             end
           ), returns, Enum.count(basis_adopters)}
      end
    end)
    |> Enum.filter(& &1)
    |> case do
      [{types, returns, _}] ->
        {types, returns}

      types ->
        select_matches(types, length(values), values)
    end
  end

  defp select_matches([], value_count, _values) do
    {Enum.map(1..value_count, fn _ -> nil end), nil}
  end

  defp select_matches(results, value_count, values) do
    case Enum.find(results, fn
           {_type, _returns, 0} ->
             true

           _ ->
             false
         end) do
      {type, returns, 0} ->
        {type, returns}

      _ ->
        results
        |> Enum.map(fn {types, {type, constraints}, _} ->
          types =
            Enum.map(types, fn {type, constraints} ->
              get_type({type, constraints})
            end)

          {types, get_type({type, constraints})}
        end)
        |> Enum.reject(fn {types, _} ->
          types
          |> Enum.zip(values)
          |> Enum.any?(fn {{type, constraints}, value} ->
            !Ash.Expr.expr?(value) and
              !(matches_type?(type, value, constraints) ||
                  match?({:ok, _}, Ash.Type.coerce(type, value, constraints)))
          end)
        end)
        |> case do
          [] ->
            {Enum.map(1..value_count, fn _ -> nil end), nil}

          results ->
            arg_types =
              1..value_count
              |> Enum.map(fn i ->
                possible_types =
                  Enum.map(results, fn {types, _} ->
                    Enum.at(types, i - 1)
                  end)

                case Enum.find(possible_types, fn {type, constraints} ->
                       matches_type?(type, Enum.at(values, i - 1), constraints)
                     end) do
                  type when not is_nil(type) ->
                    type

                  nil ->
                    case Enum.uniq_by(possible_types, &elem(&1, 0)) do
                      [single] ->
                        Enum.find(possible_types, single, fn {_, constraints} ->
                          constraints != []
                        end)

                      _ ->
                        nil
                    end
                end
              end)

            all_returns = Enum.map(results, &elem(&1, 1))

            case Enum.find_value(results, fn {types, returns} ->
                   if types == arg_types do
                     returns
                   end
                 end) do
              nil ->
                case Enum.uniq(all_returns) do
                  [single] ->
                    {arg_types, single}

                  _ ->
                    case Enum.uniq_by(all_returns, &elem(&1, 0)) do
                      [single] ->
                        {arg_types,
                         Enum.find(all_returns, single, fn {_, constraints} ->
                           constraints != []
                         end)}

                      _ ->
                        {arg_types, nil}
                    end
                end

              returns ->
                {arg_types, returns}
            end
        end
    end
  end

  def determine_type(value) do
    case value do
      %{__struct__: Ash.Query.Function.Type, arguments: [_, type, constraints]} ->
        if Ash.Type.ash_type?(type) do
          if res = get_type({type, constraints}) do
            {:ok, res}
          else
            :error
          end
        else
          :error
        end

      %{__struct__: Ash.Query.Function.Type, arguments: [_, type]} ->
        if Ash.Type.ash_type?(type) do
          if res = get_type({type, []}) do
            {:ok, res}
          else
            :error
          end
        else
          :error
        end

      %{__struct__: Ash.Query.Ref, attribute: %{type: type, constraints: constraints}} ->
        if Ash.Type.ash_type?(type) do
          if res = get_type({type, constraints}) do
            {:ok, res}
          else
            :error
          end
        else
          :error
        end

      %{__struct__: Ash.Query.Ref, attribute: %{type: type}} ->
        if Ash.Type.ash_type?(type) do
          if res = get_type({type, []}) do
            {:ok, res}
          else
            :error
          end
        else
          :error
        end

      %{__predicate__?: true} ->
        {:ok, {Ash.Type.Boolean, []}}

      %{__struct__: Ash.Query.BooleanExpression} ->
        {:ok, {Ash.Type.Boolean, []}}

      %{__struct__: Ash.Query.Exists} ->
        {:ok, {Ash.Type.Boolean, []}}

      %{__struct__: Ash.Query.Parent, expr: expr} ->
        determine_type(expr)

      %{__struct__: Ash.Query.UpsertConflict, expr: expr} ->
        determine_type(expr)

      %{__struct__: Ash.Query.Function.GetPath, arguments: [left, path]} ->
        determine_get_path_type(left, path)

      %mod{__predicate__?: _, arguments: arguments} ->
        case determine_types(mod, arguments, nil, true) do
          {_, nil} -> :error
          {_, type} -> {:ok, type}
        end

      %mod{__predicate__?: _, left: left, right: right} ->
        case determine_types(mod, [left, right], nil, true) do
          {_, nil} -> :error
          {_, type} -> {:ok, type}
        end

      _ ->
        :error
    end
  end

  defp determine_get_path_type(left, path) do
    path = List.wrap(path)

    with {:ok, {type, constraints}} <- determine_type(left),
         {:ok, {type, constraints}} <- walk_get_path({type, constraints || []}, path) do
      {:ok, {type, constraints}}
    else
      _ -> :error
    end
  end

  defp walk_get_path({type, constraints}, []) do
    {:ok, {type, constraints}}
  end

  defp walk_get_path({{:array, type}, constraints}, [segment | rest]) when is_integer(segment) do
    walk_get_path({type, get_item_constraints(constraints)}, rest)
  end

  defp walk_get_path({type, constraints}, [segment | rest]) when is_integer(segment) do
    case Ash.Type.get_type(type) do
      {:array, inner_type} ->
        walk_get_path({inner_type, get_item_constraints(constraints)}, rest)

      _ ->
        :error
    end
  end

  defp walk_get_path({type, constraints}, [segment | rest])
       when is_atom(segment) or is_binary(segment) do
    constraints = constraints || []

    cond do
      type && Ash.Type.embedded_type?(type) ->
        base_type =
          if Ash.Type.NewType.new_type?(type) do
            Ash.Type.NewType.subtype_of(type)
          else
            type
          end

        case Ash.Resource.Info.attribute(base_type, segment) do
          %{type: attr_type, constraints: attr_constraints} ->
            walk_get_path({attr_type, attr_constraints}, rest)

          _ ->
            :error
        end

      type && Ash.Type.composite?(type, constraints) ->
        case find_composite_member(type, constraints, segment) do
          {:ok, {member_type, member_constraints}} ->
            walk_get_path({member_type, member_constraints || []}, rest)

          :error ->
            :error
        end

      true ->
        :error
    end
  end

  defp walk_get_path(_type, _path), do: :error

  defp find_composite_member(type, constraints, key) do
    type
    |> Ash.Type.composite_types(constraints || [])
    |> Enum.map(fn
      {name, member_type, member_constraints} ->
        {name, nil, member_type, member_constraints}

      {name, storage_key, member_type, member_constraints} ->
        {name, storage_key, member_type, member_constraints}
    end)
    |> Enum.find(fn {name, storage_key, _member_type, _member_constraints} ->
      matches_key?(name, key) || matches_key?(storage_key, key)
    end)
    |> case do
      {_, _, member_type, member_constraints} ->
        {:ok, {member_type, member_constraints}}

      _ ->
        :error
    end
  end

  defp matches_key?(nil, _key), do: false

  defp matches_key?(key_value, key) when is_binary(key) do
    to_string(key_value) == key
  end

  defp matches_key?(key_value, key) when is_atom(key) do
    key_value == key || to_string(key_value) == Atom.to_string(key)
  end

  defp get_item_constraints(constraints) when is_list(constraints) do
    Keyword.get(constraints, :items) || []
  end

  defp get_item_constraints(_constraints), do: []

  defp get_type({type, constraints}) do
    if type = Ash.Type.get_type(type) do
      {type, constraints}
    end
  end

  defp matches_type?(type, value, constraints) do
    type = Ash.Type.get_type(type)
    Ash.Type.ash_type?(type) && Ash.Type.matches_type?(type, value, constraints)
  end

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

        atom when is_atom(atom) ->
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
