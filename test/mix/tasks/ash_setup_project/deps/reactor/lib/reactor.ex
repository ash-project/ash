# SPDX-FileCopyrightText: 2023 James Harton, Zach Daniel, Alembic Pty and contributors
# SPDX-FileCopyrightText: 2023 reactor contributors <https://github.com/ash-project/reactor/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Reactor do
  alias Reactor.{Dsl, Error.Validation.StateError, Executor, Step}

  @moduledoc """
  Reactor is a dynamic, concurrent, dependency resolving saga orchestrator.

  ## Usage

  You can construct a reactor using the `Reactor` Spark DSL:

  ```elixir
  defmodule HelloWorldReactor do
    @moduledoc false
    use Reactor

    input :whom

    step :greet, Greeter do
      argument :whom, input(:whom)
    end

    return :greet
  end
  ```

      iex> Reactor.run(HelloWorldReactor, %{whom: "Dear Reader"})
      {:ok, "Hello, Dear Reader!"}

  or you can build it programmatically:

      iex> reactor = Builder.new()
      ...> {:ok, reactor} = Builder.add_input(reactor, :whom)
      ...> {:ok, reactor} = Builder.add_step(reactor, :greet, Greeter, whom: {:input, :whom})
      ...> {:ok, reactor} = Builder.return(reactor, :greet)
      ...> Reactor.run(reactor, %{whom: nil})
      {:ok, "Hello, World!"}
  """

  defstruct context: %{},
            description: nil,
            id: nil,
            inputs: [],
            intermediate_results: %{},
            middleware: [],
            plan: nil,
            return: nil,
            state: :pending,
            steps: [],
            undo: []

  use Spark.Dsl, default_extensions: [extensions: [Dsl]]

  @type context :: %{optional(atom) => any}
  @type context_arg :: Enumerable.t({atom, any})

  @typedoc """
  Specify the maximum number of asynchronous steps which can be run in parallel.

  Defaults to the result of `System.schedulers_online/0`.  Only used if
  `async?` is set to `true`.
  """
  @type max_concurrency_option :: {:max_concurrency, pos_integer()}

  @typedoc """
  Specify the amount of execution time after which to halt processing.

  Note that this is not a hard limit. The Reactor will stop when the first step
  completes _after_ the timeout has expired.

  Defaults to `:infinity`.
  """
  @type timeout_option :: {:timeout, pos_integer() | :infinity}

  @typedoc """
  The maximum number of iterations which after which the Reactor will halt.

  Defaults to `:infinity`.
  """
  @type max_iterations_option :: {:max_iterations, pos_integer() | :infinity}

  @typedoc """
  How long to wait for asynchronous steps to complete when halting.

  Defaults to 5000ms.
  """
  @type halt_timeout_option :: {:halt_timeout, pos_integer() | :infinity}

  @typedoc """
  When set to `false` forces the Reactor to run every step synchronously,
  regardless of the step configuration.

  Defaults to `true`.
  """
  @type async_option :: {:async?, boolean}

  @typedoc """
  Use a `Reactor.Executor.ConcurrencyTracker.pool_key` to allow this Reactor to
  share it's concurrency pool with other Reactor instances.

  If you do not specify one then the Reactor will initialise a new pool and
  place it in it's context for any child Reactors to re-use.

  Only used if `async?` is set to `true`.
  """
  @type concurrency_key_option :: {:concurrency_key, reference()}

  @typedoc """
  When this option is set the Reactor will return a copy of the completed Reactor
  struct for potential future undo.
  """
  @type fully_reversible_option :: {:fully_reversible?, boolean}

  @type run_options ::
          Enumerable.t(
            max_concurrency_option
            | timeout_option
            | max_iterations_option
            | halt_timeout_option
            | async_option
            | concurrency_key_option
            | fully_reversible_option
          )

  @type undo_options :: Enumerable.t(concurrency_key_option)

  @type state :: :pending | :executing | :halted | :failed | :successful
  @type inputs :: %{optional(atom) => any}

  @type t :: %Reactor{
          context: context,
          description: nil | String.t(),
          id: any,
          inputs: [Reactor.Input.t()],
          intermediate_results: %{any => any},
          middleware: [Reactor.Middleware.t()],
          plan: nil | Graph.t(),
          undo: [{Step.t(), any}],
          return: any,
          state: state,
          steps: [Step.t()]
        }

  @doc "A guard which returns true if the value is a Reactor struct"
  @spec is_reactor(any) :: Macro.t()
  defguard is_reactor(reactor) when is_struct(reactor, __MODULE__)

  @run_schema [
    max_concurrency: [
      type: :pos_integer,
      required: false,
      doc: "The maximum number of processes to use to run the Reactor"
    ],
    timeout: [
      type: {:or, [:pos_integer, {:literal, :infinity}]},
      required: false,
      default: :infinity,
      doc: "How long to allow the Reactor to run for"
    ],
    max_iterations: [
      type: {:or, [:pos_integer, {:literal, :infinity}]},
      required: false,
      default: :infinity,
      doc: "The maximum number of times to allow the Reactor to loop"
    ],
    async?: [
      type: :boolean,
      required: false,
      default: true,
      doc: "Whether to allow the Reactor to start processes"
    ],
    concurrency_key: [
      type: :reference,
      required: false,
      hide: true
    ],
    run_id: [
      type: :any,
      required: false,
      doc: "A unique identifier for the Reactor run"
    ],
    fully_reversible?: [
      type: :boolean,
      required: false,
      default: false,
      doc: "Return the completed reactor as well as the result for possible later reversal"
    ]
  ]

  @doc """
  Attempt to run a Reactor.

  ## Arguments

  * `reactor` - The Reactor to run, either a Reactor DSL module, or a Reactor
    struct.
  * `inputs` - A map of values passed in to satisfy the Reactor's expected
    inputs.
  * `context` - An arbitrary map that will be merged into the Reactor context
    and passed into each step.

  ## Options

  #{Spark.Options.docs(@run_schema)}
  """
  @spec run(t | module, inputs, context_arg, run_options) ::
          {:ok, any} | {:ok, any, t} | {:error, any} | {:halted, t}
  def run(reactor, inputs \\ %{}, context \\ %{}, options \\ [])

  def run(reactor, inputs, context, options) when is_atom(reactor) do
    if Spark.Dsl.is?(reactor, Reactor) do
      run(reactor.reactor(), inputs, context, options)
    else
      {:error,
       ArgumentError.exception(message: "Module `#{inspect(reactor)}` is not a Reactor module")}
    end
  end

  def run(reactor, inputs, context, options)
      when is_reactor(reactor) and reactor.state in ~w[pending halted]a do
    Executor.run(reactor, inputs, context, options)
  end

  def run(reactor, _inputs, _context, _options) do
    {:error,
     StateError.exception(
       reactor: reactor,
       state: reactor.state,
       expected: ~w[pending halted]a
     )}
  end

  @doc "Raising version of `run/4`."
  @spec run!(t | module, inputs, context_arg, run_options) :: any | no_return
  def run!(reactor, inputs \\ %{}, context \\ %{}, options \\ [])

  def run!(reactor, inputs, context, options) do
    case run(reactor, inputs, context, options) do
      {:ok, value} -> value
      {:error, reason} -> raise reason
    end
  end

  @undo_options [
    concurrency_key: [
      type: :reference,
      required: false,
      hide: true
    ]
  ]

  @doc """
  Attempt to undo a previously successful Reactor.

  Undo operations are always executed sequentially in reverse completion order.
  This is intentional: while the forward execution graph captures data
  dependencies between steps, it cannot capture the full set of constraints
  that may apply during rollback. A step's undo logic may have side effects
  that affect other steps in ways not expressed in the original dependency
  graph, or external systems may have ordering constraints during rollback
  that differ from forward execution.

  Since undo operations are expected to be infrequent (only triggered on
  explicit reversal requests), sequential execution is an acceptable trade-off
  that ensures predictable rollback behaviour.

  ## Arguments

  * `reactor` - The previously successful Reactor struct.
  * `context` - An arbitrary map that will be merged into the Reactor context and passed into each undo.

  ## Options

  #{Spark.Options.docs(@undo_options)}
  """
  @spec undo(t, context_arg, undo_options) :: :ok | {:error, any}
  def undo(reactor, context, options \\ [])

  def undo(reactor, _context, _options) when not is_struct(reactor, __MODULE__) do
    {:error,
     ArgumentError.exception(
       message: "`reactor` value `#{inspect(reactor)}` is not a Reactor struct"
     )}
  end

  def undo(reactor, _context, _options) when reactor.state != :successful do
    {:error,
     StateError.exception(
       reactor: reactor,
       state: reactor.state,
       expected: ~w[successful]a
     )}
  end

  def undo(_reactor, context, _options) when not is_map(context) do
    {:error,
     ArgumentError.exception(
       message: "`context` value `#{inspect(context)}` is not valid context - must be a map"
     )}
  end

  def undo(_reactor, _context, options) when not is_list(options) do
    {:error,
     ArgumentError.exception(
       message: "`options` value `#{inspect(options)}` is not a keyword list"
     )}
  end

  def undo(reactor, context, options) do
    Reactor.Executor.undo(reactor, context, options)
  end

  @doc "A raising version of `undo/2`"
  @spec undo!(t, context_arg, undo_options) :: :ok | no_return
  def undo!(reactor, context, options) do
    with {:error, reason} <- undo(reactor, context, options) do
      raise reason
    end
  end
end
