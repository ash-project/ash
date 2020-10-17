defmodule Ash.Notifier.PubSub.Publication do
  @moduledoc "Represents an individual publication setup"

  defstruct [
    :action,
    :topic,
    :event,
    :type
  ]

  @schema [
    action: [
      type: :atom,
      doc: "The name of the action that should be published",
      required: true
    ],
    topic: [
      type: {:custom, __MODULE__, :topic, []},
      doc: "The topic to publish",
      required: true
    ],
    event: [
      type: :string,
      doc: "The name of the event to publish. Defaults to the action name"
    ],
    type: [
      type: {:one_of, [:create, :update, :destroy]},
      doc:
        "In the case of multiple actions with the same name, you may need to provide the action type as well."
    ]
  ]

  def schema, do: @schema

  @doc false
  def topic(topic) when is_binary(topic) do
    {:ok, [topic]}
  end

  def topic(topic) when is_list(topic) do
    if Enum.all?(topic, fn item -> is_binary(item) || is_atom(item) end) do
      {:ok, topic}
    else
      {:error,
       "Expected topic to be a string or a list of strings or attribute names (as atoms), got: #{
         inspect(topic)
       }"}
    end
  end

  def topic(other) do
    {:error,
     "Expected topic to be a string or a list of strings or attribute names (as atoms), got: #{
       inspect(other)
     }"}
  end
end
