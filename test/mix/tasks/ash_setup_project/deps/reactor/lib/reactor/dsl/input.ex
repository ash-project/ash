# SPDX-FileCopyrightText: 2023 James Harton, Zach Daniel, Alembic Pty and contributors
# SPDX-FileCopyrightText: 2023 reactor contributors <https://github.com/ash-project/reactor/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Reactor.Dsl.Input do
  @moduledoc """
  The struct used to store input DSL entities.

  See `d:Reactor.input`.
  """

  defstruct __identifier__: nil,
            description: nil,
            name: nil,
            transform: nil,
            __spark_metadata__: nil

  alias Reactor.{Builder, Dsl, Step}

  @type t :: %Dsl.Input{
          name: any,
          description: nil | String.t(),
          transform: {module, keyword},
          __identifier__: any,
          __spark_metadata__: Spark.Dsl.Entity.spark_meta()
        }

  @doc false
  def __entity__,
    do: %Spark.Dsl.Entity{
      name: :input,
      describe: """
      Specifies an input to the Reactor.

      An input is a value passed in to the Reactor when executing.
      If a Reactor were a function, these would be it's arguments.

      Inputs can be transformed with an arbitrary function before being passed
      to any steps.
      """,
      examples: [
        """
        input :name
        """,
        """
        input :age do
          transform &String.to_integer/1
        end
        """
      ],
      args: [:name],
      target: Dsl.Input,
      identifier: :name,
      schema: [
        name: [
          type: :atom,
          required: true,
          doc: """
          A unique name for this input. Used to allow steps to depend on it.
          """
        ],
        transform: [
          type: {:or, [{:spark_function_behaviour, Step, {Step.Transform, 1}}, nil]},
          required: false,
          default: nil,
          doc: """
          An optional transformation function which can be used to modify the input before it is passed to any steps.
          """
        ],
        description: [
          type: :string,
          required: false,
          doc: """
          An optional description for the input.
          """
        ]
      ]
    }

  defimpl Dsl.Build do
    def build(input, reactor) do
      Builder.add_input(reactor, input.name,
        description: input.description,
        transform: input.transform
      )
    end

    def verify(_input, _dsl_state), do: :ok
  end
end
