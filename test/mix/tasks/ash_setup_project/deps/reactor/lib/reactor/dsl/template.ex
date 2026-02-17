# SPDX-FileCopyrightText: 2023 James Harton, Zach Daniel, Alembic Pty and contributors
# SPDX-FileCopyrightText: 2023 reactor contributors <https://github.com/ash-project/reactor/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Reactor.Dsl.Template do
  @moduledoc """
  The `template` DSL entity struct.

  See `d:Reactor.template`.
  """

  defstruct __identifier__: nil,
            arguments: [],
            description: nil,
            guards: [],
            name: nil,
            template: nil,
            __spark_metadata__: nil

  alias Reactor.{Builder, Dsl, Step}

  @type t :: %__MODULE__{
          arguments: [Dsl.Argument.t()],
          description: nil | String.t(),
          guards: [Dsl.Where.t() | Dsl.Guard.t()],
          name: atom,
          template: String.t(),
          __identifier__: any,
          __spark_metadata__: Spark.Dsl.Entity.spark_meta()
        }

  @doc false
  def __entity__,
    do: %Spark.Dsl.Entity{
      name: :template,
      describe: """
      A step which passes all it's arguments as assigns into an `EEx` template and returns the result.
      """,
      examples: [
        ~s|
        template :welcome_message do
          arguments :user
          template """
          Welcome <%= @user.name %>! 🎉
          """
        end
        |
      ],
      args: [:name],
      target: __MODULE__,
      identifier: :name,
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
          A unique name for the step. Used when choosing the return value of the Reactor and for arguments into other steps.
          """
        ],
        description: [
          type: :string,
          required: false,
          doc: """
          An optional description for the step.
          """
        ],
        template: [
          type: :string,
          required: true,
          doc: "An `EEx` template."
        ]
      ]
    }

  defimpl Dsl.Build do
    def build(template, reactor) do
      Builder.add_step(
        reactor,
        template.name,
        {Step.Template, template: template.template},
        template.arguments,
        async?: true,
        description: template.description,
        guards: template.guards,
        max_retries: 1,
        ref: :step_name
      )
    end

    def verify(_template, _dsl_state), do: :ok
  end
end
