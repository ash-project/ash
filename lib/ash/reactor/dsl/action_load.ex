# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Reactor.Dsl.ActionLoad do
  @moduledoc """
  Add a load statement to an action.
  """

  defstruct __identifier__: nil, source: nil, transform: nil, __spark_metadata__: nil

  alias Reactor.Template
  require Template

  @type t :: %__MODULE__{
          __identifier__: any,
          source: Template.t(),
          transform: nil | (any -> any) | {module, keyword} | mfa,
          __spark_metadata__: Spark.Dsl.Entity.spark_meta()
        }

  @doc false
  def __entity__ do
    %Spark.Dsl.Entity{
      name: :load,
      describe: "Allows the addition of an Ash load statement to the action",
      args: [:source],
      imports: [Reactor.Dsl.Argument],
      identifier: {:auto, :unique_integer},
      target: __MODULE__,
      schema: [
        source: [
          type: Template.type(),
          required: true,
          doc: "What to use as the source of the load"
        ],
        transform: [
          type:
            {:or, [{:spark_function_behaviour, Reactor.Step, {Reactor.Step.Transform, 1}}, nil]},
          required: false,
          default: nil,
          doc:
            "An optional transformation function which can be used to modify the load before it is passed to the action."
        ]
      ]
    }
  end
end
