# SPDX-FileCopyrightText: 2023 James Harton, Zach Daniel, Alembic Pty and contributors
# SPDX-FileCopyrightText: 2023 reactor contributors <https://github.com/ash-project/reactor/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Reactor.Step.Group do
  @moduledoc """
  Wrap the execution of a number of steps with before/after functions.

  Unlike `Reactor.Step.Around`, this step doesn't need to run a nested Reactor
  instance, but instead can emit the steps directly into the parent Reactor.

  ## Options

  * `before` - a three-arity function that will be called before running any
    child steps.
  * `after` - a three-arity function that will be called after running any
    emitted steps.
  * `allow_async?` - a boolean indicating whether the emitted steps can be
    executed asynchronously or must remain within the current process.

  ## Before function

  The before function will be passed the following arguments:

  1. `arguments` - the values of any step arguments needed by the group.
  2. `context` - the Reactor context.
  3. `steps` - the list of steps passed in the options.

  This provides you the opportunity to modify the arguments, context and list of
  steps to be executed.

  The successful return value should be `{:ok, arguments, context, steps}`.  The
  returned arguments will be used to provide any `input` arguments to nested
  steps.

  ### Example

  ```elixir
  def no_time_travel(arguments, context, steps) do
    steps = steps
      |> Enum.filter(&(&1.name == :program_time_circuits))

    arguments = arguments
      |> Map.delete(:destination_time)

    {:ok, arguments, context, steps}
  end
  ```

  ## After function

  The after function will be called with a single argument; a map of the nested
  step results.

  The successful return value should be `{:ok, any}` where `any` will be treated
  as the result of the group.

  ```elixir
  def find_current_year(results) do
    case Map.fetch(results, :time_circuits) do
      {:ok, %{present_time: present_time}} -> {:ok, present_time.year}
      _ -> {:error, "Unable to read the present time from the time circuits"}
    end
  end
  ```
  """

  use Reactor.Step
  alias Reactor.{Argument, Builder, Step}
  import Reactor.Utils

  @behaviour Reactor.Mermaid

  @typedoc """
  The before function.
  """
  @type before_fun ::
          (Reactor.inputs(), Reactor.context(), [Step.t()] ->
             {:ok, Reactor.inputs(), Reactor.context(), [Step.t()]} | {:error, any})

  @typedoc """
  The after function.
  """
  @type after_fun :: (Reactor.inputs() -> {:ok, any} | {:error, any})

  @type options :: [before_option | after_option | allow_async_option | steps_option]

  @typedoc """
  The MFA or 3-arity function which this step will call before running any
  steps.
  """
  @type before_option :: {:before, mfa | before_fun}

  @typedoc """
  The MFA or 1-arity function which this step will call after successfully
  running the steps.
  """
  @type after_option :: {:after, mfa | after_fun}

  @typedoc """
  The initial steps to pass into the "before" function.

  Optional.
  """
  @type steps_option :: {:steps, [Step.t()]}

  @typedoc """
  Should the emitted steps be allowed to run asynchronously?

  Optional. Defaults to `true`.
  """
  @type allow_async_option :: {:allow_async?, boolean}

  @doc false
  @impl true
  @spec run(Reactor.inputs(), Reactor.context(), options) ::
          {:ok, any, [Step.t()]} | {:error, any}
  def run(arguments, context, options) do
    allow_async? = Keyword.get(options, :allow_async?, true)
    name = context.current_step.name

    with {:ok, before_fun} <- capture_before_fun(options),
         {:ok, after_fun} <- capture_after_fun(options),
         {:ok, steps} <- fetch_steps(options),
         {:ok, arguments, context, steps} <- before_fun.(arguments, context, steps),
         {:ok, reactor} <- build_nested_reactor(arguments, name, steps),
         options <-
           maybe_append_result([async?: allow_async?], fn ->
             case Map.fetch(context, :concurrency_key) do
               {:ok, value} -> {:concurrency_key, value}
               :error -> nil
             end
           end),
         {:ok, inner_result} <- Reactor.run(reactor, arguments, context, options),
         {:ok, result} <- after_fun.(inner_result) do
      {:ok, result}
    else
      {:error, reason} -> {:error, reason}
      {:halt, reactor} -> {:halt, reactor}
    end
  end

  @doc false
  @impl true
  def to_mermaid(%{impl: {__MODULE__, opts}} = step, options) do
    steps = Keyword.get(opts, :steps, [])

    with {:ok, reactor} <- build_nested_reactor(step.arguments, step.name, steps) do
      __MODULE__.Mermaid.to_mermaid(step, reactor, options)
    end
  end

  defp build_nested_reactor(arguments, name, steps) do
    reactor = Builder.new({__MODULE__, name})

    with {:ok, reactor} <- build_inputs(reactor, arguments),
         {:ok, reactor} <- build_steps(reactor, steps) do
      build_return_step(reactor, steps)
    end
  end

  defp capture_before_fun(options) do
    case Keyword.fetch(options, :before) do
      {:ok, fun} when is_function(fun, 3) ->
        {:ok, fun}

      {:ok, {m, f, []}} when is_atom(m) and is_atom(f) ->
        ensure_exported(m, f, 3, fn -> {:ok, Function.capture(m, f, 3)} end)

      {:ok, {m, f, a}} when is_atom(m) and is_atom(f) and is_list(a) ->
        ensure_exported(m, f, length(a) + 3, fn ->
          {:ok, fn arguments, context, steps -> apply(m, f, [arguments, context, steps | a]) end}
        end)

      _ ->
        {:error,
         argument_error(:options, "Expected `before` option to be a 3 arity function", options)}
    end
  end

  defp capture_after_fun(options) do
    case Keyword.fetch(options, :after) do
      {:ok, fun} when is_function(fun, 1) ->
        {:ok, fun}

      {:ok, {m, f, []}} when is_atom(m) and is_atom(f) ->
        ensure_exported(m, f, 1, fn -> {:ok, Function.capture(m, f, 1)} end)

      {:ok, {m, f, a}} when is_atom(m) and is_atom(f) and is_list(a) ->
        ensure_exported(m, f, length(a) + 1, fn ->
          {:ok, fn results -> apply(m, f, [results | a]) end}
        end)

      _ ->
        {:error,
         argument_error(:options, "Expected `after` option to be a 1 arity function", options)}
    end
  end

  defp ensure_exported(m, f, arity, callback) do
    if Code.ensure_loaded?(m) && function_exported?(m, f, arity) do
      callback.()
    else
      {:error, "Expected `#{inspect(m)}.#{f}/#{arity}` to be exported."}
    end
  end

  defp fetch_steps(options) do
    steps = Keyword.get(options, :steps, [])

    if Enum.all?(steps, &is_struct(&1, Step)) do
      {:ok, steps}
    else
      {:error,
       argument_error(
         :options,
         "Expected `steps` option to be a list of `Reactor.Step` structs",
         options
       )}
    end
  end

  defp build_inputs(reactor, arguments) do
    arguments
    |> map_while_ok(&Argument.Build.build/1)
    |> and_then(&{:ok, List.flatten(&1)})
    |> and_then(fn arguments ->
      reduce_while_ok(arguments, reactor, &Builder.add_input(&2, &1.name))
    end)
  end

  defp build_steps(reactor, steps) do
    {:ok, %{reactor | steps: Enum.concat(steps, reactor.steps)}}
  end

  defp build_return_step(reactor, steps) do
    arguments =
      steps
      |> Enum.map(&Argument.from_result(&1.name, &1.name))

    return_step_name = {__MODULE__, :return_step}

    with {:ok, reactor} <-
           Builder.add_step(reactor, return_step_name, Step.ReturnAllArguments, arguments,
             async?: false,
             max_retries: 0
           ) do
      Builder.return(reactor, return_step_name)
    end
  end
end
