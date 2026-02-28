# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Notifier.PubSub.Publication do
  @moduledoc "Represents a configured publication from the pubsub notifier on an Ash.Resource"

  defstruct [
    :action,
    :topic,
    :event,
    :type,
    :except,
    :returns,
    :constraints,
    :public?,
    :load,
    :filter,
    :transform,
    :dispatcher,
    :previous_values?,
    :__identifier__,
    :__spark_metadata__
  ]

  @schema [
    action: [
      type: :atom,
      doc: "The name of the action that should be published",
      required: true
    ],
    returns: [
      type: :any,
      doc:
        "An `Ash.Type` for the notification payload. When specified, `transform` is required and must return a value of this type."
    ],
    constraints: [
      type: :keyword_list,
      default: [],
      doc: "Constraints for the `returns` type. See `Ash.Type` for more."
    ],
    previous_values?: [
      type: :boolean,
      default: false,
      doc:
        "Whether or not to publish messages with both the new values and the old values for referencing changed attributes"
    ],
    filter: [
      type: {:fun, 1},
      doc:
        "A filter for notifications. Receives a notification, and ignores it if the function returns a falsy value."
    ],
    transform: [
      type: {:or, [{:fun, 1}, :atom]},
      doc:
        "A transformer for notifications. Either a function that receives a notification and returns a new value, " <>
          "or an atom naming a calculation on the resource whose result will be used as the broadcast value."
    ],
    topic: [
      type: {:custom, __MODULE__, :topic, []},
      doc: "The topic to publish",
      required: true
    ],
    event: [
      type: {:or, [:atom, :string]},
      doc: "The name of the event to publish. Defaults to the action name"
    ],
    public?: [
      type: :boolean,
      default: false,
      doc: "Whether or not this publication is considered public. Extensions may use this."
    ],
    load: [
      type: :any,
      doc:
        "A load statement to be applied before this publication's notification is dispatched. Loaded fields will be merged onto `notification.data`."
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
                      |> Keyword.put(:except,
                        type: {:list, :atom},
                        doc: "Exclude these actions from notifications",
                        default: []
                      )

  def schema, do: @schema
  def publish_all_schema, do: @publish_all_schema

  @doc false
  # When transform is an atom (calculation name), pass through - the verifier will validate it
  def transform(%__MODULE__{transform: transform} = publication) when is_atom(transform) and not is_nil(transform) do
    {:ok, publication}
  end

  def transform(%__MODULE__{returns: nil} = publication), do: {:ok, publication}

  def transform(%__MODULE__{returns: _returns, transform: nil}) do
    {:error, "A `transform` option is required on publications that specify `returns`"}
  end

  def transform(%__MODULE__{returns: returns, constraints: constraints} = publication) do
    resolved_type = Ash.Type.get_type(returns)

    if is_nil(resolved_type) do
      {:error, "Unknown type: #{inspect(returns)}"}
    else
      case Ash.Type.init(resolved_type, constraints || []) do
        {:ok, constraints} ->
          {:ok, %{publication | returns: resolved_type, constraints: constraints}}

        {:error, error} ->
          {:error, error}
      end
    end
  end

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
