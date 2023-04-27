defmodule Ash.Flow do
  @moduledoc """
  A flow is a static definition of a set of steps to be run.

  See the [guide](/documentation/topics/flows.md) for more.
  """

  @type t :: module

  use Spark.Dsl,
    default_extensions: [
      extensions: [Ash.Flow.Dsl]
    ]

  require Ash.Tracer

  defdelegate element_refs(input), to: Ash.Flow.Template

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

    Enum.reduce_while(arguments, {:ok, %{}}, fn arg, {:ok, acc} ->
      value =
        case Map.fetch(params, arg.name) do
          :error ->
            Map.fetch(params, to_string(arg.name))

          other ->
            other
        end

      case value do
        :error ->
          if not is_nil(arg.default) do
            value =
              case arg.default do
                {m, f, a} ->
                  apply(m, f, a)

                fun when is_function(fun, 0) ->
                  fun.()

                value ->
                  value
              end

            {:cont, {:ok, Map.put(acc, arg.name, value)}}
          else
            {:cont, {:ok, acc}}
          end

        {:ok, value} ->
          with {:ok, value} <-
                 Ash.Type.Helpers.cast_input(arg.type, value, arg.constraints, flow),
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
  @impl Spark.Dsl
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
        case Ash.Expr.eval_hydrated(hydrated) do
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

  @impl Spark.Dsl
  def explain(dsl_state, _) do
    Ash.Flow.Info.description(dsl_state)
  end
end
