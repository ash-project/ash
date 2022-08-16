defmodule Ash.Notifier.PubSub.Publication do
  @moduledoc "Represents an individual publication setup"

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
      required: true,
      links: []
    ],
    topic: [
      type: {:custom, __MODULE__, :topic, []},
      doc: "The topic to publish",
      required: true,
      links: []
    ],
    event: [
      type: :string,
      doc: "The name of the event to publish. Defaults to the action name",
      links: []
    ],
    dispatcher: [
      type: :atom,
      doc:
        "The module to use as a dispatcher. If none is set, the pubsub module provided is used.",
      links: []
    ]
  ]

  @publish_all_schema @schema
                      |> Keyword.update!(:action, &Keyword.delete(&1, :required))
                      |> Keyword.put(:type,
                        links: [],
                        type: {:in, [:create, :update, :destroy]},
                        doc: "Publish on all actions of a given type"
                      )

  def schema, do: @schema
  def publish_all_schema, do: @publish_all_schema

  @doc false
  def topic(topic) when is_binary(topic) do
    {:ok, [topic]}
  end

  def topic(topic) when is_list(topic) do
    if nested_list_of_binaries_or_atoms?(topic) do
      {:ok, topic}
    else
      {:error,
       "Expected topic to be a string or a list of strings or attribute names (as atoms), got: #{inspect(topic)}"}
    end
  end

  def topic(other) do
    {:error,
     "Expected topic to be a string or a list of strings or attribute names (as atoms), got: #{inspect(other)}"}
  end

  defp nested_list_of_binaries_or_atoms?(list) when is_list(list) do
    Enum.all?(list, &nested_list_of_binaries_or_atoms?/1)
  end

  defp nested_list_of_binaries_or_atoms?(value) when is_binary(value) or is_atom(value) do
    true
  end

  defp nested_list_of_binaries_or_atoms?(_) do
    false
  end
end
