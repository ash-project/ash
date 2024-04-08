defmodule Ash.Notifier.PubSub do
  require Logger

  @publish %Spark.Dsl.Entity{
    name: :publish,
    target: Ash.Notifier.PubSub.Publication,
    describe: "Configure a given action to publish its results over a given topic.",
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
    Works the same as `publish`, except that it takes a type and publishes all actions of that type.
    """,
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
    schema: [
      module: [
        type: :atom,
        doc: "The module to call `broadcast/3` on e.g module.broadcast(topic, event, message).",
        required: true
      ],
      prefix: [
        type: :string,
        doc:
          "A prefix for all pubsub messages, e.g `users`. A message with `created` would be published as `users:created`"
      ],
      delimiter: [
        type: :string,
        doc: "A delimiter for building topics. Default is a colon (:)"
      ],
      broadcast_type: [
        type: {:one_of, [:notification, :phoenix_broadcast, :broadcast]},
        default: :notification,
        doc: """
        What shape the event payloads will be in. See
        """
      ],
      name: [
        type: :atom,
        doc: "A named pub sub to pass as the first argument to broadcast."
      ]
    ]
  }

  @sections [@pub_sub]

  @moduledoc """
  A builtin notifier to help you publish events over any kind of pub-sub tooling.

  This is plug and play with `Phoenix.PubSub`, but could be used with any pubsub system.

  You configure a module that defines a `broadcast/3` function, and then add some "publications"
  which configure under what conditions an event should be sent and what the topic should be.

  ## Debugging PubSub

  It can be quite frustrating when setting up pub_sub when everything appears to be set up properly, but
  you aren't receiving events. This usually means some kind of mismatch between the event names produced
  by the resource/config of your publications, and you can use the following flag to display debug
  information about all pub sub events.

  ```elixir
  config :ash, :pub_sub, debug?: true
  ```

  ## Topic Templates

  Often you want to include some piece of data in the thing being changed, like the `:id` attribute. This
  is done by providing a list as the topic, and using atoms which will be replaced by their corresponding
  values. They will ultimately be joined with `:`.

  For example:

  ```elixir
  prefix "user"

  publish :create, ["created", :user_id]
  ```

  This might publish a message to "user:created:1" for example.

  For updates, if the field in the template is being changed, a message is sent
  to *both* values. So if you change `user 1` to `user 2`, the same message would
  be published to `user:updated:1` and `user:updated:2`. If there are multiple
  attributes in the template, and they are all being changed, a message is sent for
  every combination of substitutions.

  ## Important

  If the previous value was `nil` or the field was not selected on the data passed into the action, then a
  notification is not sent for the previous value.

  If the new value is `nil` then a notification is not sent for the new value.

  ## Template parts

  Templates may contain lists, in which case all combinations of values in the list will be used. Add
  `nil` to the list if you want to produce a pattern where that entry is omitted.

  The atom `:_tenant` may be used. If the changeset has a tenant set on it, that
  value will be used, otherwise that combination of values is ignored.

  The atom `:_pkey` may be used. It will be a stringified, concatenation of the primary key fields,
  or just the primary key if there is only one primary key field.

  The atom `nil` may be used. It only makes sense to use it in the context of a list of alternatives,
  and adds a pattern where that part is skipped.

  ```elixir
  publish :updated, [[:team_id, :_tenant], "updated", [:id, nil]]
  ```

  Would produce the following messages, given a `team_id` of 1, a `tenant` of `org_1`, and an `id` of `50`:

  ```elixir
  "1:updated:50"
  "1:updated"
  "org_1:updated:50"
  "org_1:updated"
  ```

  ## Custom Delimiters

  It's possible to change the default delimiter used when generating topics. This is useful when working with message brokers
  like RabbitMQ, which rely on a different set of delimiters for routing.


  ```elixir
  pub_sub do
    delimiter "."
  end
  ```

  ## Named Pubsub modules

  If you are using a phoenix `Endpoint` module for pubsub then this is unnecessary. If you want to use a custom pub sub started
  with something like `{Phoenix.PubSub, name: MyName}`, then you can provide `MyName` to here.

  ## Broadcast Types

  Configured with `broadcast_type`.

  - `:notification` just sends the notification
  - `:phoenix_broadcast` sends a `%Phoenix.Socket.Broadcast{}` (see above)
  - `:broadcast` sends `%{topic: (topic), event: (event), notification: (notification)}`
  """

  use Spark.Dsl.Extension, sections: @sections

  use Ash.Notifier

  alias Ash.Notifier.PubSub.Info

  @doc false
  def notify(%Ash.Notifier.Notification{resource: resource} = notification) do
    resource
    |> Ash.Notifier.PubSub.Info.publications()
    |> Enum.filter(&matches?(&1, notification.action))
    |> Enum.each(&publish_notification(&1, notification))
  end

  @doc false
  def requires_original_data?(resource, action) do
    resource
    |> Ash.Notifier.PubSub.Info.publications()
    |> Enum.filter(&(&1.previous_values? && matches?(&1, action)))
    |> Enum.flat_map(fn publish ->
      publish.topic
      |> List.flatten()
      |> Enum.filter(&is_atom/1)
    end)
    |> Enum.any?(&Ash.Resource.Info.attribute(resource, &1))
  end

  defp publish_notification(publish, notification) do
    debug? = Application.get_env(:ash, :pub_sub)[:debug?] || false
    event = publish.event || to_string(notification.action.name)
    prefix = Ash.Notifier.PubSub.Info.prefix(notification.resource) || ""
    delimiter = Info.delimiter(notification.resource)

    topics =
      publish.topic
      |> fill_template(notification, delimiter, publish.previous_values?)
      |> Enum.map(fn topic ->
        case {prefix, topic} do
          {"", ""} -> ""
          {prefix, ""} -> prefix
          {"", topic} -> topic
          {prefix, topic} -> "#{prefix}#{delimiter}#{topic}"
        end
      end)

    if debug? do
      Logger.debug("""
      Broadcasting to topics #{inspect(topics)} via #{inspect(Ash.Notifier.PubSub.Info.module(notification.resource))}.broadcast

      Notification:

      #{inspect(notification)}
      """)
    end

    Enum.each(topics, fn topic ->
      args =
        case Ash.Notifier.PubSub.Info.name(notification.resource) do
          nil ->
            [topic, event, to_payload(topic, event, notification)]

          pub_sub ->
            payload = to_payload(topic, event, notification)

            [pub_sub, topic, payload]
        end

      args =
        case publish.dispatcher do
          nil ->
            args

          dispatcher ->
            args ++ dispatcher
        end

      apply(Ash.Notifier.PubSub.Info.module(notification.resource), :broadcast, args)
    end)
  end

  def to_payload(topic, event, notification) do
    case Ash.Notifier.PubSub.Info.broadcast_type(notification.resource) do
      :phoenix_broadcast ->
        %{
          __struct__: Phoenix.Socket.Broadcast,
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

  defp fill_template(topic, _notification, _delimiter, _previous_values?) when is_binary(topic),
    do: [topic]

  defp fill_template(topic, notification, delimiter, previous_values?) do
    topic
    |> all_combinations_of_values(notification, notification.action.type, previous_values?)
    |> Enum.map(&List.flatten/1)
    |> Enum.map(&Enum.join(&1, delimiter))
    |> Enum.uniq()
  end

  defp all_combinations_of_values(
         items,
         notification,
         action_type,
         _previous_values?,
         trail \\ []
       )

  defp all_combinations_of_values([], _, _, _previous_values?, trail), do: [Enum.reverse(trail)]

  defp all_combinations_of_values(
         [nil | rest],
         notification,
         action_type,
         previous_values?,
         trail
       ) do
    all_combinations_of_values(rest, notification, action_type, previous_values?, trail)
  end

  defp all_combinations_of_values(
         [item | rest],
         notification,
         action_type,
         previous_values?,
         trail
       )
       when is_binary(item) do
    all_combinations_of_values(rest, notification, action_type, previous_values?, [item | trail])
  end

  defp all_combinations_of_values(
         [:_tenant | rest],
         notification,
         action_type,
         previous_values?,
         trail
       ) do
    if notification.changeset.tenant do
      all_combinations_of_values(rest, notification, action_type, previous_values?, [
        notification.changeset.tenant | trail
      ])
    else
      []
    end
  end

  defp all_combinations_of_values([:_pkey | rest], notification, type, true, trail)
       when type in [:update, :destroy] do
    pkey = Ash.Resource.Info.primary_key(notification.changeset.resource)
    pkey_value_before_change = Enum.map_join(pkey, "-", &Map.get(notification.changeset.data, &1))
    pkey_value_after_change = Enum.map_join(pkey, "-", &Map.get(notification.data, &1))

    [pkey_value_before_change, pkey_value_after_change]
    |> Enum.uniq()
    |> Enum.flat_map(fn possible_value ->
      all_combinations_of_values(rest, notification, type, true, [
        possible_value | trail
      ])
    end)
  end

  defp all_combinations_of_values([:_pkey | rest], notification, type, _, trail)
       when type in [:update, :destroy] do
    pkey = Ash.Resource.Info.primary_key(notification.changeset.resource)
    pkey_value_after_change = Enum.map_join(pkey, "-", &Map.get(notification.data, &1))

    all_combinations_of_values(rest, notification, type, false, [pkey_value_after_change | trail])
  end

  defp all_combinations_of_values(
         [:_pkey | rest],
         notification,
         action_type,
         previous_values?,
         trail
       ) do
    pkey = notification.changeset.resource

    all_combinations_of_values(rest, notification, action_type, previous_values?, [
      Enum.map_join(pkey, "-", &Map.get(notification.data, &1)) | trail
    ])
  end

  defp all_combinations_of_values([item | rest], notification, type, true, trail)
       when is_atom(item) and type in [:update, :destroy] do
    value_before_change = Map.get(notification.changeset.data, item)
    value_after_change = Map.get(notification.data, item)

    [value_before_change, value_after_change]
    |> Enum.filter(&publishable_value?(&1, notification))
    |> Enum.uniq()
    |> Enum.flat_map(fn possible_value ->
      all_combinations_of_values(rest, notification, type, true, [possible_value | trail])
    end)
  end

  defp all_combinations_of_values([item | rest], notification, type, _, trail)
       when is_atom(item) and type in [:update, :destroy] do
    value_after_change = Map.get(notification.data, item)

    if publishable_value?(value_after_change, notification) do
      all_combinations_of_values(rest, notification, type, false, [value_after_change | trail])
    else
      []
    end
  end

  defp all_combinations_of_values(
         [item | rest],
         notification,
         action_type,
         previous_values?,
         trail
       )
       when is_atom(item) do
    value = Map.get(notification.data, item)

    if publishable_value?(value, notification) do
      all_combinations_of_values(rest, notification, action_type, previous_values?, [
        value | trail
      ])
    else
      []
    end
  end

  defp all_combinations_of_values(
         [item | rest],
         notification,
         action_type,
         previous_values?,
         trail
       )
       when is_list(item) do
    Enum.flat_map(item, fn possible_value ->
      all_combinations_of_values(
        [possible_value | rest],
        notification,
        action_type,
        previous_values?,
        trail
      )
    end)
  end

  defp publishable_value?(nil, _notification), do: false

  defp publishable_value?(%Ash.NotLoaded{field: field}, notification) do
    Logger.warning(
      "Not publishing notification `#{inspect(notification.topic)}` for #{inspect(notification.resource)} because `#{field}` is not loaded"
    )

    false
  end

  defp publishable_value?(%Ash.ForbiddenField{field: field}, notification) do
    Logger.warning(
      "Not publishing notification `#{inspect(notification.topic)}` for #{inspect(notification.resource)} because `#{field}` is an `%Ash.ForbiddenField{}`"
    )

    false
  end

  defp publishable_value?(_, _), do: true

  defp matches?(%{action: action}, %{name: action}), do: true
  defp matches?(%{type: type}, %{type: type}), do: true

  defp matches?(_, _), do: false
end
