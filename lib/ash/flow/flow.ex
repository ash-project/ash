defmodule Ash.Flow do
  @moduledoc """
  A flow is a static definition of a set of steps to be .

  See the {{link:ash:guide:Flows}} guide for more.
  """

  @type t :: module

  use Spark.Dsl,
    default_extensions: [
      extensions: [Ash.Flow.Dsl]
    ]

  require Ash.Tracer

  @spec run!(any, any, Keyword.t()) :: Ash.Flow.Result.t() | no_return()
  def run!(flow, input, opts \\ []) do
    case run(flow, input, opts) do
      %Ash.Flow.Result{valid?: true} = result ->
        result

      %Ash.Flow.Result{errors: errors} ->
        raise Ash.Error.to_error_class(errors)
    end
  end

  @spec run(any, any, Keyword.t()) :: Ash.Flow.Result.t()
  def run(flow, input, opts \\ []) do
    params = input
    executor = opts[:executor] || Ash.Flow.Executor.AshEngine

    opts =
      opts
      |> add_actor()
      |> add_tenant()
      |> add_tracer()

    Ash.Tracer.span :flow, Ash.Flow.Info.trace_name(flow), opts[:tracer] do
      Ash.Tracer.telemetry_span [:ash, :flow], %{
        flow_short_name: Ash.Flow.Info.short_name(flow)
      } do
        with {:ok, input} <- cast_input(flow, input),
             {:ok, built} <- executor.build(flow, input, opts) do
          case executor.execute(built, input, opts) do
            {:ok, result, metadata} ->
              %Ash.Flow.Result{
                flow: flow,
                params: params,
                input: input,
                result: result,
                notifications: metadata[:notifications] || [],
                valid?: true,
                complete?: true
              }

            {:ok, result} ->
              %Ash.Flow.Result{
                flow: flow,
                params: params,
                input: input,
                result: result,
                valid?: true,
                complete?: true
              }

            {:error, metadata, error} ->
              complete? = complete?(error)

              %Ash.Flow.Result{
                flow: flow,
                params: params,
                input: input,
                notifications: metadata[:notifications] || [],
                runner_metadata: if(not complete?, do: metadata[:runner_metadata]),
                valid?: false,
                complete?: complete?,
                errors: List.wrap(error)
              }

            {:error, error} ->
              %Ash.Flow.Result{
                flow: flow,
                params: params,
                input: input,
                valid?: false,
                complete?: complete?(error),
                errors: List.wrap(error)
              }
          end
        else
          {:error, new_input, error} ->
            %Ash.Flow.Result{
              flow: flow,
              params: input,
              input: new_input,
              valid?: false,
              complete?: complete?(error),
              errors: List.wrap(error)
            }

          {:error, error} ->
            %Ash.Flow.Result{
              flow: flow,
              params: input,
              input: input,
              valid?: false,
              complete?: complete?(error),
              errors: List.wrap(error)
            }
        end
      end
    end
  end

  defp complete?(%Ash.Error.Flow.Halted{}) do
    false
  end

  defp complete?(_) do
    true
  end

  defp add_actor(opts) do
    if Keyword.has_key?(opts, :actor) do
      opts
    else
      case Process.get(:ash_actor) do
        {:actor, value} ->
          Keyword.put(opts, :actor, value)

        _ ->
          opts
      end
    end
  end

  defp add_tenant(opts) do
    if Keyword.has_key?(opts, :actor) do
      opts
    else
      case Process.get(:ash_tenant) do
        {:tenant, value} ->
          Keyword.put(opts, :tenant, value)

        _ ->
          opts
      end
    end
  end

  defp add_tracer(opts) do
    if Keyword.has_key?(opts, :tracer) do
      opts
    else
      case Process.get(:ash_tracer) do
        {:tracer, value} ->
          Keyword.put(opts, :tracer, value || Application.get_env(:ash, :tracer))

        _ ->
          Keyword.put(opts, :tracer, Application.get_env(:ash, :tracer))
      end
    end
  end

  defp cast_input(flow, params) do
    arguments = Ash.Flow.Info.arguments(flow)

    Enum.reduce_while(params, {:ok, %{}}, fn {name, value}, {:ok, acc} ->
      case Enum.find(arguments, &(&1.name == name || to_string(&1.name) == name)) do
        nil ->
          {:cont, {:ok, acc}}

        arg ->
          with {:ok, value} <- Ash.Changeset.cast_input(arg.type, value, arg.constraints, flow),
               {:constrained, {:ok, casted}}
               when not is_nil(value) <-
                 {:constrained, Ash.Type.apply_constraints(arg.type, value, arg.constraints)} do
            {:cont, {:ok, Map.put(acc, arg.name, casted)}}
          else
            {:constrained, {:ok, nil}} ->
              {:cont, {:ok, Map.put(acc, arg.name, nil)}}

            {:constrained, {:error, error}} ->
              {:halt, {:error, acc, error}}

            {:error, error} ->
              {:halt, {:error, acc, error}}
          end
      end
    end)
  end

  @doc false
  # sobelow_skip ["DOS.StringToAtom"]
  def handle_before_compile(_opts) do
    quote bind_quoted: [] do
      {opt_args, args} =
        __MODULE__
        |> Ash.Flow.Info.arguments()
        |> Enum.split_with(&(&1.allow_nil? || !is_nil(&1.default)))

      args = Enum.map(args, & &1.name)

      opt_args = Enum.map(opt_args, & &1.name)

      arg_vars = Enum.map(args, &{&1, [], Elixir})
      @doc Ash.Flow.Info.description(__MODULE__)

      def run!(unquote_splicing(arg_vars), input \\ %{}, opts \\ []) do
        {input, opts} =
          if opts == [] && Keyword.keyword?(input) do
            {%{}, input}
          else
            {input, opts}
          end

        opt_input =
          Enum.reduce(unquote(opt_args), input, fn opt_arg, input ->
            case Map.fetch(input, opt_arg) do
              {:ok, val} ->
                Map.put(input, opt_arg, val)

              :error ->
                case Map.fetch(input, to_string(opt_arg)) do
                  {:ok, val} ->
                    Map.put(input, opt_arg, val)

                  :error ->
                    input
                end
            end
          end)

        required_input =
          unquote(args)
          |> Enum.zip([unquote_splicing(arg_vars)])
          |> Map.new()

        all_input = Map.merge(required_input, opt_input)

        Ash.Flow.run!(__MODULE__, all_input, opts)
      end

      def run(unquote_splicing(arg_vars), input \\ %{}, opts \\ []) do
        {input, opts} =
          if opts == [] && Keyword.keyword?(input) do
            {%{}, input}
          else
            {input, opts}
          end

        opt_input =
          Enum.reduce(unquote(opt_args), input, fn opt_arg, input ->
            case Map.fetch(input, opt_arg) do
              {:ok, val} ->
                Map.put(input, opt_arg, val)

              :error ->
                case Map.fetch(input, to_string(opt_arg)) do
                  {:ok, val} ->
                    Map.put(input, opt_arg, val)

                  :error ->
                    input
                end
            end
          end)

        required_input =
          unquote(args)
          |> Enum.zip([unquote_splicing(arg_vars)])
          |> Map.new()

        all_input = Map.merge(required_input, opt_input)

        Ash.Flow.run(__MODULE__, all_input, opts)
      end

      @default_short_name __MODULE__
                          |> Module.split()
                          |> Enum.reverse()
                          |> Enum.take(2)
                          |> Enum.reverse()
                          |> Enum.map_join("_", &Macro.underscore/1)
                          |> String.to_atom()

      def default_short_name do
        @default_short_name
      end
    end
  end

  def handle_modifiers(action_input) do
    do_handle_modifiers(action_input)
  end

  defp do_handle_modifiers(action_input)
       when is_map(action_input) and not is_struct(action_input) do
    Map.new(action_input, fn {key, value} ->
      new_key = do_handle_modifiers(key)
      new_val = do_handle_modifiers(value)
      {new_key, new_val}
    end)
  end

  defp do_handle_modifiers(action_input) when is_list(action_input) do
    Enum.map(action_input, &do_handle_modifiers(&1))
  end

  defp do_handle_modifiers({:_path, value, path}) do
    do_get_in(do_handle_modifiers(value), path)
  end

  defp do_handle_modifiers({:_expr, expr}) do
    case Ash.Filter.hydrate_refs(expr, %{
           resource: nil,
           aggregates: %{},
           calculations: %{},
           public?: false
         }) do
      {:ok, hydrated} ->
        case Ash.Filter.Runtime.do_match(nil, hydrated) do
          {:ok, result} ->
            result

          :unknown ->
            raise """
            Expression #{inspect(expr)} could not be evaluated in the context of the flow
            """

          {:error, error} ->
            raise Ash.Error.to_error_class(error)
        end

      {:error, error} ->
        raise Ash.Error.to_error_class(error)
    end
  end

  defp do_handle_modifiers(action_input) when is_tuple(action_input) do
    List.to_tuple(do_handle_modifiers(Tuple.to_list(action_input)))
  end

  defp do_handle_modifiers(other), do: other

  @doc false
  def do_get_in(value, []), do: value

  def do_get_in(value, [key | rest]) when is_atom(key) and is_struct(value) do
    do_get_in(Map.get(value, key), rest)
  end

  def do_get_in(value, [key | rest]) do
    do_get_in(get_in(value, [key]), rest)
  end

  def do_fetch_in(value, []), do: {:ok, value}

  def do_fetch_in(value, [key | rest]) when is_atom(key) and is_struct(value) do
    case Map.fetch(value, key) do
      {:ok, value} ->
        do_fetch_in(value, rest)

      :error ->
        :error
    end
  end

  def do_fetch_in(value, [key | rest]) when is_map(value) do
    case Map.fetch(value, key) do
      {:ok, result} ->
        do_fetch_in(result, rest)

      :error ->
        :error
    end
  end

  def do_fetch_in(_, _), do: :error

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
    {:_expr, Ash.Filter.walk_filter_template(expr, &do_remap_result_references(&1, prefix))}
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
    {:_expr, Ash.Filter.walk_filter_template(expr, &do_set_dependent_values(&1, input))}
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
    {Map.get(input, name) || Map.get(input, to_string(name)), []}
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
