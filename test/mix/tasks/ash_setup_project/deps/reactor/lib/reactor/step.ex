# SPDX-FileCopyrightText: 2023 James Harton, Zach Daniel, Alembic Pty and contributors
# SPDX-FileCopyrightText: 2023 reactor contributors <https://github.com/ash-project/reactor/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Reactor.Step do
  @moduledoc """
  The Step behaviour and struct.

  Implement this behaviour to make steps for your Reactor.
  """

  defstruct arguments: [],
            async?: true,
            context: %{},
            description: nil,
            impl: nil,
            name: nil,
            max_retries: :infinity,
            ref: nil,
            transform: nil,
            guards: []

  alias Reactor.{Argument, Step}

  @type context :: %{optional(atom) => any}
  @type arguments :: %{optional(atom) => any}

  @type t :: %Step{
          arguments: [Argument.t()],
          async?: boolean | (keyword -> boolean),
          context: context(),
          description: nil | String.t(),
          impl: module | {module, keyword},
          name: any,
          max_retries: non_neg_integer() | :infinity,
          ref: nil | reference(),
          transform: nil | (any -> any) | {module, keyword} | mfa,
          guards: [Reactor.Guard.t()]
        }

  @type step :: module

  @typedoc """
  Optional capabilities which may be implemented by the step module.

  This allows us to optimise out calls steps which cannot be undone, etc.
  """
  @type capability :: :compensate | :undo

  @typedoc """
  Possible valid return values for the `c:run/3` callback.
  """
  @type run_result ::
          {:ok, value :: any}
          | {:ok, value :: any, [Step.t()]}
          | :retry
          | {:halt | :error | :retry, reason :: any}

  @typedoc """
  Possible valid return values for the `c:compensate/4` callback.
  """
  @type compensate_result ::
          {:continue, value :: any} | :ok | :retry | {:error | :retry, reason :: any}

  @typedoc """
  Possible valid return values for the `c:undo/4` callback.
  """
  @type undo_result :: :ok | :retry | {:retry | :error, reason :: any}

  @typedoc """
  Possible valid return values for the `c:backoff/4` callback.
  """
  @type backoff_result :: :now | (millis :: pos_integer())

  @doc """
  Execute the step.

  This is the function that implements the behaviour you wish to execute.  You
  will receive arguments as per the `t:Step.t` definition along with their
  corresponding values as a map and a copy of the current reactor context.

  ## Arguments

    - `arguments` - A map of arguments as per the `t:Step.t` definition we're
      called from.
    - `context` - The reactor context.
    - `options` - A keyword list of options provided to the step (if any).

  ## Return values

    - `{:ok, value}` the step completed successfully it returns the value in an
      ok tuple.
    - `{:ok, value, [step]}` the step completed successfully and wants to add
      new steps to the reactor.
    - `{:error, reason}` the if step failed, return an error tuple.
    - `:retry` or `{:retry, reason}` the step failed, but is retryable.  You can
      optionally supply an error reason which will be used in the event that the
      step runs out of retries, otherwise a `Reactor.Error.RetriesExceededError`
      will be used.
    - `{:halt, reason}` terminate (or pause) reactor execution.  If there are
      actively running steps the reactor will wait for them to finish and then
      return the incomplete state for later resumption.
  """
  @callback run(
              arguments :: Reactor.inputs(),
              context :: Reactor.context(),
              options :: keyword
            ) :: run_result

  @doc """
  Compensate for the failure of the step.

  > Do not implement this callback if your step doesn't support compensation.

  If `run/3` returned an error then this callback will be called the error
  reason and the original arguments.

  This provides you the opportunity to handle the error in a number of ways and
  direct the reactor as to what to do next.

  ## Arguments
    - `reason` - the error reason returned from `c:run/3`.
    - `arguments` - the arguments passed to the step.
    - `context` - the reactor context.
    - `options` - a keyword list of options provided to the step (if any).

  ## Return values

    - `{:continue, value}` if you're able to provide a valid result for the step
      (perhaps by re-running the original computation) then return that within a
      `:continue` tuple and execution will continue as planned.
    - `:ok` the step was successfully compensated and the reactor should
      continue undoing upstream changes.
    - `:retry` or `{:retry, reason}` if you would like the reactor to attempt to
      re-run the step. You can optionally supply an error reason which will be
      used in the event that the step runs out of retries, otherwise a
      `Reactor.Error.Invalid.RetriesExceededError` will be used.
    - `{:error, reason}` if compensation was unsuccessful.
  """
  @callback compensate(
              reason :: any,
              arguments :: Reactor.inputs(),
              context :: Reactor.context(),
              options :: keyword
            ) :: compensate_result

  @doc """
  Undo a previously successful execution of the step.

  > Do not implement this callback if your step doesn't support undoing.

  This callback is called when the reactor encounters an unhandled error later
  in it's execution run and must undo the work previously done.

  ## Arguments

    - `value` - the return value of the previously successful call to `c:run/3`.
    - `arguments` - the arguments passed to the step.
    - `context` - the reactor context.
    - `options` - a keyword list of options provided to the step (if any).

  ## Return values

    - `:ok` the step was successfully undo and the reactor should continue
      rolling back.
    - `{:error, reason}` there was an error while attempting to compensate.  The
      reactor will collect the error and continue rolling back.
    - `:retry` if you would like the reactor to attempt to undo the again later
      - possibly in the case of a network failure for example.
  """
  @callback undo(
              value :: any,
              arguments :: Reactor.inputs(),
              Reactor.context(),
              options :: keyword
            ) :: undo_result

  @doc """
  Detect the capabilities of the step at runtime.

  > This callback is automatically defined by `use Reactor.Step` however you're
  > free to override it if you need specific behaviour.

  Whenever Reactor would like to either undo a change made by the step, or
  compensate a step failure this callback is called to detect whether the step
  module is capable of the desired action.

  The default implementation of this callback checks to see if the optional
  callback is defined on the current module.
  """
  @callback can?(step :: Step.t(), capability()) :: boolean

  @doc """
  Detect if the step can be run asynchronously at runtime.

  > This callback is automatically defined by `use Reactor.Step` however you're
  > free to override it if you need a specific behaviour.

  This callback is called when Reactor is deciding whether to run a step
  asynchronously.

  The default implementation of this callback checks returns the the value of
  the steps's `async?` key if it is boolean, or calls it with the steps's
  options if it is a function.
  """
  @callback async?(step :: Step.t()) :: boolean

  @doc """
  Extract nested steps from the step's options.

  > This callback is automatically defined by `use Reactor.Step` however you're
  > free to override it if you need specific behaviour.

  This callback is called during the planning phase to extract any nested steps
  that may be contained within this step's options. This allows the planner to
  identify cross-scope dependencies and properly track them in the dependency graph.

  The default implementation returns an empty list.

  ## Arguments

    - `options` - the keyword list of options provided to the step.

  ## Return values

    - A list of nested `Reactor.Step` structs contained within this step.
  """
  @callback nested_steps(options :: keyword) :: [Step.t()]

  @doc """
  Generate a backoff time (in milliseconds) before the step is retried.

  > This callback is automatically defined by `use Reactor.Step` however you should
  > override it if you wish to perform any step backoff on retry.

  This callback is called when Reactor is scheduling a retry. If a positive integer
  is returned then Reactor will wait **at least** as many milliseconds before
  calling the step's `run/3` callback again.  If `:now` is returned then the step
  is available to the next scheduler run.

  The default implementation returns `:now`, meaning that the step is retried
  immediately without any backoff.

  ## Arguments
    - `reason` - the error or retry reason returned from `c:run/3` or `c:compensate/4`.
    - `arguments` - the arguments passed to the step.
    - `context` - the reactor context.
    - `options` - a keyword list of options provided to the step (if any).

  ## Return values

    - `:now` - the step should be retried immediately.
    - a possitive integer expressing a minimum delay in milliseconds. 
  """
  @callback backoff(
              reason :: nil | any,
              arguments :: Reactor.inputs(),
              context :: Reactor.context(),
              options :: keyword
            ) :: backoff_result()

  @optional_callbacks backoff: 4, compensate: 4, undo: 4, nested_steps: 1

  @doc """
  Find out of a step has a capability.
  """
  @spec can?(Step.t(), capability()) :: boolean
  def can?(step, capability) when is_struct(step, Step) and capability in ~w[undo compensate]a,
    do:
      module_and_options_from_step(step, fn module, _options -> module.can?(step, capability) end)

  @doc """
  Execute a step.
  """
  @spec run(Step.t(), arguments :: Reactor.inputs(), context :: Reactor.context()) :: run_result()
  def run(step, arguments, context),
    do:
      module_and_options_from_step(step, fn module, options ->
        module.run(arguments, context, options)
      end)

  @doc """
  Compensate a step
  """
  @spec compensate(
          Step.t(),
          reason :: any,
          arguments :: Reactor.inputs(),
          context :: Reactor.context()
        ) :: compensate_result()
  def compensate(step, reason, arguments, context),
    do:
      module_and_options_from_step(step, fn module, options ->
        module.compensate(reason, arguments, context, options)
      end)

  @doc """
  Undo a step
  """
  @spec undo(Step.t(), value :: any, arguments :: Reactor.inputs(), context :: Reactor.context()) ::
          undo_result()
  def undo(step, value, arguments, context),
    do:
      module_and_options_from_step(step, fn module, options ->
        module.undo(value, arguments, context, options)
      end)

  @doc """
  Is the step able to be run asynchronously?
  """
  @spec async?(Step.t()) :: boolean
  def async?(step),
    do: module_and_options_from_step(step, fn module, _opts -> module.async?(step) end)

  @doc """
  Extract nested steps from a step.
  """
  @spec nested_steps(Step.t()) :: [Step.t()]
  def nested_steps(step) do
    module_and_options_from_step(step, fn module, options ->
      if function_exported?(module, :nested_steps, 1) do
        module.nested_steps(options)
      else
        []
      end
    end)
  end

  @doc """
  Generate the backoff for a step
  """
  @spec backoff(
          Step.t(),
          reason :: any,
          arguments :: Reactor.inputs(),
          context :: Reactor.context()
        ) :: backoff_result()
  def backoff(step, reason, arguments, context),
    do:
      module_and_options_from_step(step, fn module, options ->
        module.backoff(reason, arguments, context, options)
      end)

  defp module_and_options_from_step(%{impl: {module, options}} = step, fun)
       when is_struct(step, Step) and is_atom(module) and is_list(options) and is_function(fun, 2),
       do: fun.(module, options)

  defp module_and_options_from_step(%{impl: module} = step, fun)
       when is_struct(step, Step) and is_atom(module) and is_function(fun, 2),
       do: fun.(module, [])

  @doc false
  @spec __using__(keyword) :: Macro.output()
  defmacro __using__(_opts) do
    quote do
      @behaviour unquote(__MODULE__)

      @doc false
      @impl unquote(__MODULE__)
      def can?(_step, capability), do: function_exported?(__MODULE__, capability, 4)

      @doc false
      @impl unquote(__MODULE__)
      def async?(step) when is_boolean(step.async?), do: step.async?

      def async?(%{async?: fun, impl: {_, opts}}) when is_function(fun, 1),
        do: fun.(opts)

      def async?(%{async?: fun}) when is_function(fun, 1), do: fun.([])
      def async?(_), do: false

      @doc false
      @impl unquote(__MODULE__)
      def nested_steps(_options), do: []

      @doc false
      @impl unquote(__MODULE__)
      def backoff(_, _, _, _), do: :now

      defoverridable can?: 2, async?: 1, nested_steps: 1, backoff: 4
    end
  end
end
