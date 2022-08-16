defmodule Ash.Notifier.PubSub do
  require Logger

  @publish %Spark.Dsl.Entity{
    name: :publish,
    target: Ash.Notifier.PubSub.Publication,
    describe: """
    Configure a given action to publish its results over a given topic.
    """,
    links: [
      guides: [
        "ash:guide:PubSub",
        "ash:guide:Notifiers"
      ]
    ],
    examples: [
      "publish :create, \"created\"",
      """
      publish :assign, "assigned"
      """
    ],
    schema: Ash.Notifier.PubSub.Publication.schema(),
    args: [:action, :topic]
  }

  @publish_all %Spark.Dsl.Entity{
    name: :publish_all,
    target: Ash.Notifier.PubSub.Publication,
    describe: """
    Works just like `publish`, except that it takes a type
    and publishes all actions of that type
    """,
    links: [
      guides: [
        "ash:guide:PubSub",
        "ash:guide:Notifiers"
      ]
    ],
    examples: [
      "publish_all :create, \"created\""
    ],
    schema: Ash.Notifier.PubSub.Publication.publish_all_schema(),
    args: [:type, :topic]
  }

  @pub_sub %Spark.Dsl.Section{
    name: :pub_sub,
    describe: """
    A section for configuring how resource actions are published over pubsub
    """,
    examples: [
      """
      pub_sub do
        module MyEndpoint
        prefix "post"

        publish :destroy, ["foo", :id]
        publish :update, ["bar", :name] event: "name_change"
        publish_all :create, "created"
      end
      """
    ],
    entities: [
      @publish,
      @publish_all
    ],
    no_depend_modules: [:module],
    links: [
      guides: [
        "ash:guide:PubSub",
        "ash:guide:Notifiers"
      ]
    ],
    schema: [
      module: [
        type: :atom,
        doc: "The module to call `broadcast/3` on e.g module.broadcast(topic, event, message).",
        required: true,
        links: []
      ],
      prefix: [
        type: :string,
        doc:
          "A prefix for all pubsub messages, e.g `users`. A message with `created` would be published as `users:created`",
        links: []
      ],
      broadcast_type: [
        type: {:one_of, [:notification, :phoenix_broadcast, :broadcast]},
        default: :notification,
        doc: """
        What shape the event payloads will be in. `:notification` just sends the notification, `phoenix_broadcast` sends a `%Phoenix.Socket.Broadcast{}`, and `:broadcast`
        sends `%{topic: <topic>, event: <event>, notification: <notification>}`
        """,
        links: []
      ],
      name: [
        type: :atom,
        doc: """
        A named pub sub to pass as the first argument to broadcast.

        If you are simply using your `Endpoint` module for pubsub then this is unnecessary. If you want to use
        a custom pub started with something like `{Phoenix.PubSub, name: MyName}`, then you can provide `MyName` to
        here.
        """,
        links: []
      ]
    ]
  }

  @sections [@pub_sub]

  @moduledoc """
  A pubsub notifier extension
  """

  use Spark.Dsl.Extension, sections: @sections

  @deprecated "use Ash.Notifier.PubSub.Info.publications/1 instead"
  defdelegate publications(resource), to: Ash.Notifier.PubSub.Info

  @deprecated "use Ash.Notifier.PubSub.Info.module/1 instead"
  defdelegate module(resource), to: Ash.Notifier.PubSub.Info

  @deprecated "use Ash.Notifier.PubSub.Info.prefix/1 instead"
  defdelegate prefix(resource), to: Ash.Notifier.PubSub.Info

  @deprecated "use Ash.Notifier.PubSub.Info.name/1 instead"
  defdelegate name(resource), to: Ash.Notifier.PubSub.Info

  @doc false
  def notify(%Ash.Notifier.Notification{resource: resource} = notification) do
    resource
    |> publications()
    |> Enum.filter(&matches?(&1, notification))
    |> Enum.each(&publish_notification(&1, notification))
  end

  defp publish_notification(publish, notification) do
    debug? = Application.get_env(:ash, :pub_sub)[:debug?] || false
    event = publish.event || to_string(notification.action.name)
    prefix = prefix(notification.resource) || ""

    topics = fill_template(publish.topic, notification)

    if debug? do
      Logger.debug("""
      Broadcasting to topics #{inspect(topics)} via #{inspect(module(notification.resource))}.broadcast

      Notification:

      #{inspect(notification)}
      """)
    end

    Enum.each(topics, fn topic ->
      prefixed_topic = prefix <> ":" <> topic

      args =
        case name(notification.resource) do
          nil ->
            [prefixed_topic, event, to_payload(topic, event, notification)]

          pub_sub ->
            payload = to_payload(topic, event, notification)

            [pub_sub, prefixed_topic, payload]
        end

      args =
        case publish.dispatcher do
          nil ->
            args

          dispatcher ->
            args ++ dispatcher
        end

      apply(module(notification.resource), :broadcast, args)
    end)
  end

  if Code.ensure_loaded?(Phoenix.Socket.Broadcast) do
    def to_payload(topic, event, notification) do
      case Ash.Notifier.PubSub.Info.broadcast_type(notification.resource) do
        :phoenix_broadcast ->
          %Phoenix.Socket.Broadcast{
            topic: topic,
            event: event,
            payload: notification
          }

        :broadcast ->
          %{
            topic: topic,
            event: event,
            payload: notification
          }

        :notification ->
          notification
      end
    end
  else
    def to_payload(topic, event, notification) do
      case Ash.Notifier.PubSub.Info.broadcast_type(notification.resource) do
        :phoenix_broadcast ->
          raise "A resource was configured with `broadcast_type :phoenix_broadcast` but `Phoenix.Socket.Broadcast` was not compiled."

        :broadcast ->
          %{
            topic: topic,
            event: event,
            payload: notification
          }

        :notification ->
          notification
      end
    end
  end

  defp fill_template(topic, _) when is_binary(topic), do: [topic]

  defp fill_template(topic, notification) do
    topic
    |> all_combinations_of_values(notification, notification.action.type)
    |> Enum.map(&List.flatten/1)
    |> Enum.map(&Enum.join(&1, ":"))
  end

  defp all_combinations_of_values(items, notification, action_type, trail \\ [])

  defp all_combinations_of_values([], _, _, trail), do: [Enum.reverse(trail)]

  defp all_combinations_of_values([nil | rest], notification, action_type, trail) do
    all_combinations_of_values(rest, notification, action_type, trail)
  end

  defp all_combinations_of_values([item | rest], notification, action_type, trail)
       when is_binary(item) do
    all_combinations_of_values(rest, notification, action_type, [item | trail])
  end

  defp all_combinations_of_values([:_tenant | rest], notification, action_type, trail) do
    if notification.changeset.tenant do
      all_combinations_of_values(rest, notification, action_type, [
        notification.changeset.tenant | trail
      ])
    else
      []
    end
  end

  defp all_combinations_of_values([:_pkey | rest], notification, type, trail)
       when type in [:update, :destroy] do
    pkey = Ash.Resource.Info.primary_key(notification.changeset.resource)
    pkey_value_before_change = Enum.map_join(pkey, "-", &Map.get(notification.changeset.data, &1))
    pkey_value_after_change = Enum.map_join(pkey, "-", &Map.get(notification.data, &1))

    [pkey_value_before_change, pkey_value_after_change]
    |> Enum.reject(&is_nil/1)
    |> Enum.uniq()
    |> Enum.flat_map(fn possible_value ->
      all_combinations_of_values(rest, notification, type, [possible_value | trail])
    end)
  end

  defp all_combinations_of_values([:_pkey | rest], notification, action_type, trail) do
    pkey = notification.changeset.resource

    all_combinations_of_values(rest, notification, action_type, [
      Enum.map_join(pkey, "-", &Map.get(notification.data, &1)) | trail
    ])
  end

  defp all_combinations_of_values([item | rest], notification, type, trail)
       when is_atom(item) and type in [:update, :destroy] do
    value_before_change = Map.get(notification.changeset.data, item)
    value_after_change = Map.get(notification.data, item)

    [value_before_change, value_after_change]
    |> Enum.reject(&is_nil/1)
    |> Enum.uniq()
    |> Enum.flat_map(fn possible_value ->
      all_combinations_of_values(rest, notification, type, [possible_value | trail])
    end)
  end

  defp all_combinations_of_values([item | rest], notification, action_type, trail)
       when is_atom(item) do
    all_combinations_of_values(rest, notification, action_type, [
      Map.get(notification.data, item) | trail
    ])
  end

  defp all_combinations_of_values([item | rest], notification, action_type, trail)
       when is_list(item) do
    Enum.flat_map(item, fn possible_value ->
      all_combinations_of_values([possible_value | rest], notification, action_type, trail)
    end)
  end

  defp matches?(%{action: action}, %{action: %{name: action}}), do: true
  defp matches?(%{type: type}, %{action: %{type: type}}), do: true

  defp matches?(_, _), do: false
end
