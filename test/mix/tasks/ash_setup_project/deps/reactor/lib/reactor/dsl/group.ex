# SPDX-FileCopyrightText: 2023 James Harton, Zach Daniel, Alembic Pty and contributors
# SPDX-FileCopyrightText: 2023 reactor contributors <https://github.com/ash-project/reactor/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Reactor.Dsl.Group do
  @moduledoc """
  The `group` DSL entity struct.

  See `d:Reactor.group`.
  """
  defstruct __identifier__: nil,
            after_all: nil,
            allow_async?: false,
            arguments: [],
            before_all: nil,
            description: nil,
            guards: [],
            name: nil,
            steps: [],
            __spark_metadata__: nil

  alias Reactor.{Builder, Dsl, Step}

  @type t :: %Dsl.Group{
          __identifier__: any,
          after_all: mfa | Step.Group.after_fun(),
          allow_async?: true,
          arguments: [Dsl.Argument.t()],
          before_all: mfa | Step.Group.before_fun(),
          description: nil | String.t(),
          guards: [Dsl.Where.t() | Dsl.Guard.t()],
          name: atom,
          steps: [Dsl.Step.t()],
          __spark_metadata__: Spark.Dsl.Entity.spark_meta()
        }

  @doc false
  def __entity__,
    do: %Spark.Dsl.Entity{
      name: :group,
      describe: """
      Call functions before and after a group of steps.
      """,
      target: Dsl.Group,
      args: [:name],
      identifier: :name,
      entities: [
        arguments: [Dsl.Argument.__entity__(), Dsl.WaitFor.__entity__()],
        guards: [Dsl.Where.__entity__(), Dsl.Guard.__entity__()],
        steps: []
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
        before_all: [
          type: {:mfa_or_fun, 3},
          required: true,
          doc: """
          The before function. See `Reactor.Step.Group` for more information.
          """
        ],
        after_all: [
          type: {:mfa_or_fun, 1},
          required: true,
          doc: """
          The after function. See `Reactor.Step.Group` for more information.
          """
        ],
        allow_async?: [
          type: :boolean,
          required: false,
          default: true,
          doc: """
          Whether the emitted steps should be allowed to run asynchronously.
          """
        ],
        description: [
          type: :string,
          required: false,
          doc: """
          An optional description for the step.
          """
        ]
      ]
    }

  defimpl Dsl.Build do
    import Reactor.Utils
    alias Spark.{Dsl.Verifier, Error.DslError}

    def build(group, reactor) do
      sub_reactor = Builder.new(reactor.id)

      with {:ok, sub_reactor} <- build_inputs(sub_reactor, group),
           {:ok, sub_reactor} <- build_steps(sub_reactor, group) do
        Builder.add_step(
          reactor,
          group.name,
          {Step.Group,
           before: group.before_all,
           after: group.after_all,
           steps: sub_reactor.steps,
           allow_async?: group.allow_async?},
          group.arguments,
          async?: group.allow_async?,
          description: group.description,
          guards: group.guards,
          max_retries: 0,
          ref: :step_name
        )
      end
    end

    def verify(group, dsl_state) when group.steps == [] do
      {:error,
       DslError.exception(
         module: Verifier.get_persisted(dsl_state, :module),
         path: [:reactor, :group, group.name],
         message: "Group contains no steps"
       )}
    end

    def verify(group, dsl_state) do
      group.steps
      |> Enum.reduce_while(:ok, fn step, :ok ->
        case Dsl.Build.verify(step, dsl_state) do
          :ok -> {:cont, :ok}
          {:error, reason} -> {:halt, {:error, reason}}
        end
      end)
    end

    defp build_inputs(reactor, group) do
      group.arguments
      |> Enum.map(& &1.name)
      |> reduce_while_ok(reactor, &Builder.add_input(&2, &1))
    end

    defp build_steps(reactor, group) do
      group.steps
      |> reduce_while_ok(reactor, &Dsl.Build.build/2)
    end
  end
end
