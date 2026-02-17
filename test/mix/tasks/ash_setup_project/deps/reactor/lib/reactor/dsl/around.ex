# SPDX-FileCopyrightText: 2023 James Harton, Zach Daniel, Alembic Pty and contributors
# SPDX-FileCopyrightText: 2023 reactor contributors <https://github.com/ash-project/reactor/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Reactor.Dsl.Around do
  @moduledoc """
  The `around` DSL entity struct.

  See `d:Reactor.around`.
  """
  defstruct __identifier__: nil,
            allow_async?: false,
            arguments: [],
            description: nil,
            fun: nil,
            guards: [],
            name: nil,
            steps: [],
            __spark_metadata__: nil

  alias Reactor.{Builder, Dsl, Step}

  @type t :: %Dsl.Around{
          __identifier__: any,
          allow_async?: boolean,
          arguments: [Dsl.Argument.t()],
          description: nil | String.t(),
          fun: mfa | Step.Around.around_fun(),
          guards: [Dsl.Where.t() | Dsl.Guard.t()],
          name: atom,
          steps: [Dsl.Step.t()],
          __spark_metadata__: Spark.Dsl.Entity.spark_meta()
        }

  @doc false
  def __entity__,
    do: %Spark.Dsl.Entity{
      name: :around,
      describe: """
      Wrap a function around a group of steps.
      """,
      target: Dsl.Around,
      args: [:name, {:optional, :fun}],
      identifier: :name,
      entities: [
        steps: [],
        arguments: [Dsl.Argument.__entity__(), Dsl.WaitFor.__entity__()],
        guards: [Dsl.Where.__entity__(), Dsl.Guard.__entity__()]
      ],
      recursive_as: :steps,
      schema: [
        name: [
          type: :atom,
          required: true,
          doc: """
          A unique name for the group of steps.
          """
        ],
        description: [
          type: :string,
          required: false,
          doc: """
          An optional description for the step.
          """
        ],
        fun: [
          type: {:mfa_or_fun, 4},
          required: true,
          doc: """
          The around function. See `Reactor.Step.Around` for more information.
          """
        ],
        allow_async?: [
          type: :boolean,
          required: false,
          default: false,
          doc: """
          Whether the emitted steps should be allowed to run asynchronously.
          """
        ]
      ]
    }

  defimpl Dsl.Build do
    import Reactor.Utils
    alias Spark.{Dsl.Verifier, Error.DslError}

    def build(around, reactor) do
      sub_reactor = Builder.new(reactor.id)

      with {:ok, sub_reactor} <- build_inputs(sub_reactor, around),
           {:ok, sub_reactor} <- build_steps(sub_reactor, around) do
        Builder.add_step(
          reactor,
          around.name,
          {Step.Around,
           steps: sub_reactor.steps, fun: around.fun, allow_async?: around.allow_async?},
          around.arguments,
          async?: around.allow_async?,
          description: around.description,
          guards: around.guards,
          max_retries: 0,
          ref: :step_name
        )
      end
    end

    def verify(around, dsl_state) when around.steps == [] do
      {:error,
       DslError.exception(
         module: Verifier.get_persisted(dsl_state, :module),
         path: [:reactor, :around, around.name],
         message: "Around contains no steps"
       )}
    end

    def verify(around, dsl_state) do
      around.steps
      |> Enum.reduce_while(:ok, fn step, :ok ->
        case Dsl.Build.verify(step, dsl_state) do
          :ok -> {:cont, :ok}
          {:error, reason} -> {:halt, {:error, reason}}
        end
      end)
    end

    defp build_inputs(reactor, around) do
      around.arguments
      |> Enum.map(& &1.name)
      |> reduce_while_ok(reactor, &Builder.add_input(&2, &1))
    end

    defp build_steps(reactor, around) do
      around.steps
      |> reduce_while_ok(reactor, &Dsl.Build.build/2)
    end
  end
end
