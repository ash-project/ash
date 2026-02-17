# SPDX-FileCopyrightText: 2023 James Harton, Zach Daniel, Alembic Pty and contributors
# SPDX-FileCopyrightText: 2023 reactor contributors <https://github.com/ash-project/reactor/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Reactor.Step.Recurse do
  @moduledoc """
  A built-in step which can recursively execute a reactor.

  This step executes a reactor repeatedly, using the output of each execution
  as the input for the next, until either:

  1. The exit condition is met
  2. The maximum number of iterations is reached
  3. An error occurs

  Reactor will correctly share the concurrency availability between iterations.
  """

  use Reactor.Step
  @behaviour Reactor.Mermaid

  @option_schema [
    state: [
      type: {:in, [:init, :iterating, :complete]},
      required: true,
      doc: """
      The current execution state of the recursion.
      """
    ],
    reactor: [
      type: {:or, [{:struct, Reactor}, :atom]},
      required: true,
      doc: """
      The reactor to execute recursively.
      """
    ],
    max_iterations: [
      type: :pos_integer,
      required: false,
      doc: """
      The maximum number of iterations to perform.
      """
    ],
    exit_condition: [
      type: {:or, [nil, {:mfa_or_fun, 1}]},
      required: false,
      doc: """
      A function that takes the result of an iteration and returns whether to exit the recursion.
      """
    ],
    current_iteration: [
      type: :non_neg_integer,
      required: false,
      default: 0,
      doc: """
      The current iteration count.
      """
    ],
    previous_result: [
      type: :any,
      required: false,
      doc: """
      The result of the previous iteration, used to check exit conditions and feed into the next iteration.
      """
    ]
  ]

  @doc false
  @impl true
  def run(arguments, context, options) do
    with {:ok, options} <- Spark.Options.validate(options, @option_schema) do
      case options[:state] do
        :init -> do_init(arguments, context, options)
        :iterating -> do_iterate(arguments, context, options)
        :complete -> {:ok, options[:previous_result]}
      end
    end
  end

  defp do_init(arguments, context, options) do
    # First time execution of the reactor
    reactor = options[:reactor]

    Reactor.run(reactor, arguments, context,
      concurrency_key: context.concurrency_key,
      async?: options[:async?] || false
    )
    |> handle_iteration_result(context, options)
  end

  defp do_iterate(_arguments, context, options) do
    # Subsequent executions using previous results
    reactor = options[:reactor]

    # Use the previous result as the arguments for this iteration
    Reactor.run(reactor, options[:previous_result], context,
      concurrency_key: context.concurrency_key,
      async?: options[:async?] || false
    )
    |> handle_iteration_result(context, options)
  end

  defp handle_iteration_result({:ok, result}, context, options) do
    current_iteration = options[:current_iteration] || 0
    next_iteration = current_iteration + 1
    max_iterations = options[:max_iterations]
    exit_condition = options[:exit_condition]

    cond do
      # Check exit condition if provided
      exit_condition && exit_condition.(result) ->
        {:ok, result}

      # Check max iterations if provided
      max_iterations && next_iteration >= max_iterations ->
        {:ok, result}

      # Continue iteration
      true ->
        next_options =
          options
          |> Keyword.put(:state, :iterating)
          |> Keyword.put(:current_iteration, next_iteration)
          |> Keyword.put(:previous_result, result)

        # Create a recursive step to continue the iteration
        recursive_step =
          Reactor.Builder.new_step!(
            context.current_step.name,
            {__MODULE__, next_options}
          )

        {:ok, result, [recursive_step]}
    end
  end

  defp handle_iteration_result(error, _context, _options) do
    # If the inner reactor execution fails, propagate the error
    error
  end

  @doc false
  @impl true
  def to_mermaid(step, options), do: __MODULE__.Mermaid.to_mermaid(step, options)
end
