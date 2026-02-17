# SPDX-FileCopyrightText: 2023 James Harton, Zach Daniel, Alembic Pty and contributors
# SPDX-FileCopyrightText: 2023 reactor contributors <https://github.com/ash-project/reactor/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Reactor.Step.Around do
  @moduledoc """
  Wrap the execution of a number of steps in a function.

  This allows you to provide custom context and filter the provided steps as
  needed.

  ## Options

  * `fun` - a four-arity function that will be called when executing this step.
  * `steps` - a list of steps which are will be provided to the above mentioned
    function.
  * `allow_async?` - a boolean indicating whether the nested steps can be
    executed asynchronously or must remain within the current process.

  ## Wrapper function

  Your around function will be called by this step and will be passed the
  following arguments:

  * `arguments` - the arguments passed to the step.
  * `context` - the context passed to the step.
  * `steps` - the list of steps passed in the options.
  * `callback` - a 3 arity function that you can call to execute steps.

  This provides you the opportunity to modify the arguments, context and list of
  steps to be executed.  You then can call the callback with the modified
  arguments, context and steps and they will be executed in a Reactor of their
  own.  The callback will return `{:ok, results}` where results is a map of all
  of the step results by name, or an error tuple.

  You can then modify the result in any way before returning it as the return of
  the around step.

  ## Callback function

  The callback function will spawn a separate Reactor and run provided steps to
  completion using `arguments` as input.

  It expects the following three arguments to be passed:

  1. `arguments` - a map of arguments to be used as input to the nested Reactor.
  2. `context` - the context passed to the nested Reactor.
  3. `steps` - the list of steps which will be executed in the nested Reactor.

  ## Example

  You could use a function like that below to cause some steps to be executed
  inside an Ecto database transaction.

  ```elixir
  def in_transaction(arguments, context, steps, callback) do
    MyApp.Repo.transaction(fn ->
      case callback.(arguments, context, steps) do
        {:ok, results} -> result
        {:error, reason} -> raise reason
      end
    end)
  end
  ```
  """

  use Reactor.Step
  alias Reactor.{Argument, Builder, Step}
  import Reactor.Utils
  @behaviour Reactor.Mermaid

  @typedoc """
  The type signature for the provided callback function.
  """
  @type callback ::
          (Reactor.inputs(), Reactor.context(), [Step.t()] -> {:ok, any} | {:error, any})

  @typedoc """
  The type signature for the "around" function.
  """
  @type around_fun ::
          (Reactor.inputs(), Reactor.context(), [Step.t()], callback ->
             {:ok, any} | {:error, any})

  @type options :: [function_option | steps_option | allow_async_option]

  @typedoc """
  The MFA or 4-arity function which this step will call.
  """
  @type function_option :: {:fun, mfa | around_fun}

  @typedoc """
  The initial steps to pass into the "around" function.

  Optional.
  """
  @type steps_option :: {:steps, [Step.t()]}

  @typedoc """
  Should the inner Reactor be allowed to run tasks asynchronously?

  Optional. Defaults to `true`.
  """
  @type allow_async_option :: {:allow_async?, boolean}

  @doc false
  @impl true
  @spec run(Reactor.inputs(), Reactor.context(), options) :: {:ok, any} | {:error, any}
  def run(arguments, context, options) do
    with {:ok, fun} <- capture(options),
         {:ok, steps} <- fetch_steps(options),
         {:ok, result} <- fun.(arguments, context, steps, &__MODULE__.around(&1, &2, &3, options)) do
      {:ok, result}
    else
      {:error, reason} -> {:error, reason}
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

  @doc false
  def around(_arguments, _context, [], _options), do: {:ok, %{}}

  def around(arguments, context, steps, options) do
    allow_async? = Keyword.get(options, :allow_async?, true)

    with {:ok, reactor} <- build_nested_reactor(arguments, context.current_step.name, steps) do
      options =
        maybe_append_result([async?: allow_async?], fn ->
          case Map.fetch(context, :concurrency_key) do
            {:ok, value} -> {:concurrency_key, value}
            :error -> nil
          end
        end)

      Reactor.run(reactor, arguments, context, options)
    end
  end

  defp build_nested_reactor(arguments, name, steps) do
    reactor = Builder.new({__MODULE__, name})

    with {:ok, reactor} <- build_inputs(reactor, arguments),
         {:ok, reactor} <- build_steps(reactor, steps) do
      build_return_step(reactor, steps)
    end
  end

  defp capture(options) do
    case Keyword.fetch(options, :fun) do
      {:ok, fun} when is_function(fun, 4) ->
        {:ok, fun}

      {:ok, {m, f, []}} when is_atom(m) and is_atom(f) ->
        ensure_exported(m, f, 4, fn -> {:ok, Function.capture(m, f, 4)} end)

      {:ok, {m, f, a}} when is_atom(m) and is_atom(f) and is_list(a) ->
        ensure_exported(m, f, length(a) + 4, fn ->
          {:ok,
           fn arguments, context, steps, callback ->
             apply(m, f, [arguments, context, steps, callback | a])
           end}
        end)

      {:ok, _} ->
        {:error,
         argument_error(:options, "Expected `fun` option to be a 4 arity function", options)}

      :error ->
        {:error, argument_error(:options, "The required option `fun` is not present", options)}
    end
  end

  defp ensure_exported(m, f, arity, callback) do
    if Code.ensure_loaded?(m) && function_exported?(m, f, arity) do
      callback.()
    else
      {:error,
       argument_error(
         :mfa,
         "Expected `#{inspect(m)}.#{f}/#{arity}` to be exported.",
         {m, f, arity}
       )}
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
