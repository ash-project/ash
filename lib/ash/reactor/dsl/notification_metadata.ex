# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Reactor.Dsl.NotificationMetadata do
  @moduledoc """
  Specify the notification metadata for an action.
  """

  defstruct __identifier__: nil, source: nil, transform: nil, __spark_metadata__: nil

  alias Reactor.Template

  @type t :: %__MODULE__{
          __identifier__: any,
          source: Template.t() | map() | nil,
          transform: nil | (any -> any) | {module, keyword} | mfa,
          __spark_metadata__: Spark.Dsl.Entity.spark_meta()
        }

  @doc false
  def __entity__ do
    template_type = Template.type()

    %Spark.Dsl.Entity{
      name: :notification_metadata,
      describe: "Specifies metadata to be merged into the metadata field for all notifications sent from this operation",
      args: [:source],
      imports: [Reactor.Dsl.Argument],
      identifier: {:auto, :unique_integer},
      target: __MODULE__,
      schema: [
        source: [
          type: {:or, [template_type, :map, nil]},
          required: true,
          doc: "What to use as the source of the notification metadata."
        ],
        transform: [
          type:
            {:or, [{:spark_function_behaviour, Reactor.Step, {Reactor.Step.Transform, 1}}, nil]},
          required: false,
          default: nil,
          doc:
            "An optional transformation function which can be used to modify the notification metadata before it is passed to the action."
        ]
      ]
    }
  end
end
