defmodule Ash.Reactor.Dsl.Actor do
  @moduledoc """
  Specify the actor used to execute an action.
  """

  defstruct __identifier__: nil, source: nil, transform: nil

  alias Reactor.Template

  @type t :: %__MODULE__{
          __identifier__: any,
          source: Template.Input.t() | Template.Result.t() | Template.Value.t(),
          transform: nil | (any -> any) | {module, keyword} | mfa
        }

  @doc false
  def __entity__ do
    template_type = Template.type()

    %Spark.Dsl.Entity{
      name: :actor,
      describe: "Specifies the action actor",
      args: [:source],
      imports: [Reactor.Dsl.Argument],
      identifier: {:auto, :unique_integer},
      target: __MODULE__,
      schema: [
        source: [
          type: template_type,
          required: true,
          doc: "What to use as the source of the actor."
        ],
        transform: [
          type:
            {:or, [{:spark_function_behaviour, Reactor.Step, {Reactor.Step.Transform, 1}}, nil]},
          required: false,
          default: nil,
          doc:
            "An optional transformation function which can be used to modify the actor before it is passed to the action."
        ]
      ]
    }
  end
end
