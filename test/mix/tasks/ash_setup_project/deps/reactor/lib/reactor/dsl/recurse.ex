# SPDX-FileCopyrightText: 2023 James Harton, Zach Daniel, Alembic Pty and contributors
# SPDX-FileCopyrightText: 2023 reactor contributors <https://github.com/ash-project/reactor/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Reactor.Dsl.Recurse do
  @moduledoc """
  The `recurse` DSL entity struct.

  See the `d:Reactor.recurse`.
  """
  defstruct __identifier__: nil,
            arguments: [],
            async?: nil,
            description: nil,
            guards: [],
            name: nil,
            reactor: nil,
            max_iterations: nil,
            exit_condition: nil,
            __spark_metadata__: nil

  alias Reactor.{Builder, Dsl}
  alias Spark.Dsl.Transformer

  @type t :: %Dsl.Recurse{
          __identifier__: any,
          arguments: [Dsl.Argument.t() | Dsl.WaitFor.t()],
          async?: nil | boolean,
          description: nil | String.t(),
          guards: [Dsl.Where.t() | Dsl.Guard.t()],
          name: any,
          reactor: module | Reactor.t(),
          max_iterations: nil | pos_integer(),
          exit_condition: nil | (any -> boolean),
          __spark_metadata__: Spark.Dsl.Entity.spark_meta()
        }

  @doc false
  def __entity__,
    do: %Spark.Dsl.Entity{
      name: :recurse,
      describe: """
      Execute a reactor recursively until an exit condition is met or maximum iterations are reached.

      Allows the output of one iteration to become the input of the next. The reactor must have
      input/output compatibility to ensure that the outputs from iteration i can be fed into
      iteration i+1.
      """,
      args: [:name, :reactor],
      target: Dsl.Recurse,
      identifier: :name,
      no_depend_modules: [:reactor],
      entities: [
        arguments: [Dsl.Argument.__entity__(), Dsl.WaitFor.__entity__()],
        guards: [Dsl.Where.__entity__(), Dsl.Guard.__entity__()]
      ],
      recursive_as: :steps,
      schema: [
        name: [
          type: :atom,
          required: true,
          doc: """
          A unique name for the step. Allows the result of the recursed reactor to be depended upon by steps in this reactor.
          """
        ],
        description: [
          type: :string,
          required: false,
          doc: """
          An optional description for the step.
          """
        ],
        reactor: [
          type: {:or, [{:struct, Reactor}, {:spark, Reactor.Dsl}]},
          required: true,
          doc: """
          The reactor module or struct to recurse on.
          """
        ],
        async?: [
          type: :boolean,
          required: false,
          default: true,
          doc: """
          Whether the recursed steps should be run asynchronously.
          """
        ],
        max_iterations: [
          type: :pos_integer,
          required: false,
          doc: """
          The maximum number of iterations to execute. If not specified, will rely solely on exit_condition.
          """
        ],
        exit_condition: [
          type: {:or, [nil, {:mfa_or_fun, 1}]},
          required: false,
          doc: """
          A function that takes the result of an iteration and returns a boolean. When true, the recursion will stop. If not provided, will rely solely on max_iterations.
          """
        ]
      ]
    }

  defimpl Dsl.Build do
    alias Spark.{Dsl.Verifier, Error.DslError}

    def build(step, reactor) do
      Builder.recurse(reactor, step.name, step.reactor, step.arguments,
        async?: step.async?,
        description: step.description,
        guards: step.guards,
        max_iterations: step.max_iterations,
        exit_condition: step.exit_condition
      )
    end

    def verify(step, dsl_state) do
      with :ok <- verify_arguments(step, dsl_state) do
        verify_constraints(step, dsl_state)
      end
    end

    defp verify_arguments(step, dsl_state) do
      case Builder.Compose.verify_arguments(step.reactor, step.arguments) do
        :ok ->
          :ok

        {:error, {:extra_args, inputs, extra_args}} ->
          {:error,
           %DslError{
             module: Verifier.get_persisted(dsl_state, :module),
             path: [:reactor, :recurse, step.name],
             message: """
             # Extra arguments while recursing Reactors.

             The recursed Reactor takes the following inputs:

             #{Enum.map_join(inputs, "\n", &"  - #{&1}")}

             The extra arguments are:

             #{Enum.map_join(extra_args, "\n", &"  - #{&1}")}
             """
           }}

        {:error, {:missing_args, inputs, missing_args}} ->
          {:error,
           %DslError{
             module: Verifier.get_persisted(dsl_state, :module),
             path: [:reactor, :recurse, step.name],
             message: """
             # Missing arguments while recursing Reactors.

             The recursed Reactor takes the following inputs:

             #{Enum.map_join(inputs, "\n", &"  - #{&1}")}

             The missing arguments are:

             #{Enum.map_join(missing_args, "\n", &"  - #{&1}")}
             """
           }}
      end
    end

    defp verify_constraints(step, dsl_state) do
      with :ok <- verify_termination_conditions(step, dsl_state) do
        verify_reactor_return(step, dsl_state)
      end
    end

    defp verify_termination_conditions(step, dsl_state) do
      if is_nil(step.max_iterations) and is_nil(step.exit_condition) do
        {:error,
         %DslError{
           module: Verifier.get_persisted(dsl_state, :module),
           path: [:reactor, :recurse, step.name],
           message: """
           # Missing constraints for recursion.

           You must provide at least one of:
             - max_iterations: a maximum number of iterations to prevent infinite loops
             - exit_condition: a function that determines when to stop recursion
           """
         }}
      else
        :ok
      end
    end

    defp verify_reactor_return(step, dsl_state) do
      return_section = Transformer.get_option(dsl_state, [:reactor], :return)

      if is_nil(return_section) do
        {:error,
         %DslError{
           module: Verifier.get_persisted(dsl_state, :module),
           path: [:reactor, :recurse, step.name],
           message: """
           # Missing return value in recursed reactor.

           The reactor being recursed must have a return value defined to enable iterative execution.
           """
         }}
      else
        :ok
      end
    end
  end
end
