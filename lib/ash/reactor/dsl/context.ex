defmodule Ash.Reactor.Dsl.Context do
  @moduledoc """
  Specify context to merge into an action's context.
  """

  defstruct __identifier__: nil, context: nil, transform: nil

  alias Reactor.Template

  @type t :: %__MODULE__{
          __identifier__: any,
          context: nil | Ash.Reactor.Dsl.Context.t() | map,
          transform: nil | (any -> any) | {module, keyword} | mfa
        }

  @doc false
  def __entity__ do
    %Spark.Dsl.Entity{
      name: :context,
      describe: "A map to be merged into the action's context",
      args: [:context],
      imports: [Reactor.Dsl.Argument],
      identifier: {:auto, :unique_integer},
      target: __MODULE__,
      schema: [
        context: [
          type: {:or, [nil, Template.type(), :map]},
          required: false,
          doc: "A map to be merged into the action's context."
        ],
        transform: [
          type:
            {:or, [{:spark_function_behaviour, Reactor.Step, {Reactor.Step.Transform, 1}}, nil]},
          required: false,
          default: nil,
          doc:
            "An optional transformation function which can be used to modify the context before it is passed to the action."
        ]
      ]
    }
  end
end
