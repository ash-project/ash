# SPDX-FileCopyrightText: 2023 James Harton, Zach Daniel, Alembic Pty and contributors
# SPDX-FileCopyrightText: 2023 reactor contributors <https://github.com/ash-project/reactor/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Reactor.Dsl.Debug do
  @moduledoc """
  The `debug` DSL entity struct.

  See `d:Reactor.debug`.
  """

  defstruct __identifier__: nil,
            arguments: [],
            description: nil,
            guards: [],
            level: :debug,
            name: nil,
            __spark_metadata__: nil

  alias Reactor.Dsl.{Argument, Build, Debug, Guard, WaitFor, Where}

  @type t :: %Debug{
          __identifier__: any,
          arguments: [Argument.t()],
          description: nil | String.t(),
          guards: [Where.t() | Guard.t()],
          level: Logger.level(),
          name: atom,
          __spark_metadata__: Spark.Dsl.Entity.spark_meta()
        }

  @doc false
  def __entity__,
    do: %Spark.Dsl.Entity{
      name: :debug,
      describe: """
      Inserts a step which will send debug information to the `Logger`.
      """,
      examples: [
        """
        debug :debug do
          argument :suss, result(:suss_step)
        end
        """
      ],
      target: Debug,
      args: [:name],
      identifier: :name,
      entities: [
        arguments: [Argument.__entity__(), WaitFor.__entity__()],
        guards: [Where.__entity__(), Guard.__entity__()]
      ],
      recursive_as: :steps,
      schema: [
        name: [
          type: :atom,
          required: true,
          doc: """
          A unique identifier for the step.
          """
        ],
        level: [
          type: {:in, [:emergency, :alert, :critical, :error, :warning, :notice, :info, :debug]},
          required: false,
          default: :debug,
          doc: """
          The log level to send the debug information to.
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

  defimpl Build do
    alias Reactor.{Builder, Step}

    def build(debug, reactor) do
      Builder.add_step(
        reactor,
        debug.name,
        {Step.Debug, level: debug.level},
        debug.arguments,
        description: debug.description,
        guards: debug.guards,
        max_retries: 0,
        ref: :step_name
      )
    end

    def verify(_, _), do: :ok
  end
end
