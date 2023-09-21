defmodule Ash.Notifier.PubSub.Publication do
  @moduledoc "Represents a configured publication from the pubsub notifier on an Ash.Resource"

  defstruct [
    :action,
    :topic,
    :event,
    :type,
    :dispatcher
  ]

  @schema [
    action: [
      type: :atom,
      doc: "The name of the action that should be published",
      required: true
    ],
    topic: [
      type: {:wrap_list, {:or, [:string, :atom]}},
      doc: "The topic to publish",
      required: true
    ],
    event: [
      type: :string,
      doc: "The name of the event to publish. Defaults to the action name"
    ],
    dispatcher: [
      type: :atom,
      doc:
        "The module to use as a dispatcher. If none is set, the pubsub module provided is used."
    ]
  ]

  @publish_all_schema @schema
                      |> Keyword.update!(:action, &Keyword.delete(&1, :required))
                      |> Keyword.put(:type,
                        type: {:in, [:create, :update, :destroy]},
                        doc: "Publish on all actions of a given type"
                      )

  def schema, do: @schema
  def publish_all_schema, do: @publish_all_schema
end
