defmodule Ash.Notifier.PubSub do
  require Logger

  @publish %Ash.Dsl.Entity{
    name: :publish,
    target: Ash.Notifier.PubSub.Publication,
    describe: """
    Configure a given action to publish its results over a given topic.

    If you have multiple actions with the same name (only possible if they have different types),
    use the `type` option, to specify which type you are referring to. Otherwise the message will
    be broadcast for all actions with that name.

    To include attribute values of the resource in the message, pass a list
    of strings and attribute names. They will ultimately be joined with `:`.
    For example:

    ```elixir
    prefix "user"

    publish :create, ["created", :user_id]
    ```

    This might publish a message to \"user:created:1\"" for example.

    For updates, if the field in the template is being changed, a message is sent
    to *both* values. So if you change `user 1` to `user 2`, the same message would
    be published to `user:updated:1` and `user:updated:2`. If there are multiple
    attributes in the template, and they are all being changed, a message is sent for
    every combination of substitutions.

    ## Template parts

    Templates may contain lists, in which case all combinations of values in the list will be used. Add
    `nil` to the list if you want to produce a pattern where that entry is ommitted.

    The atom `:_tenant` may be used. If the changeset has a tenant set on it, that
    value will be used, otherwise that combination of values is ignored.

    The atom `:_pkey` may be used. It will be a stringified, concatenation of the primary key fields,
    or just the primary key if there is only one primary key field.

    The atom `:_skip` may be used. It only makes sense to use it in the context of a list of alternatives,
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
    """,
    examples: [
      "publish :create, \"created\"",
      """
      publish :assign, "assigned"
      """
    ],
    schema: Ash.Notifier.PubSub.Publication.schema(),
    args: [:action, :topic]
  }

  @publish_all %Ash.Dsl.Entity{
    name: :publish_all,
    target: Ash.Notifier.PubSub.Publication,
    describe: """
    Works just like `publish`, except that it takes a type
    and publishes all actions of that type
    """,
    examples: [
      "publish_all :create, \"created\""
    ],
    schema: Ash.Notifier.PubSub.Publication.publish_all_schema(),
    args: [:type, :topic]
  }

  @pub_sub %Ash.Dsl.Section{
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
      name: [
        type: :atom,
        doc: """
        A named pub sub to pass as the first argument to broadcast.

        If you are simply using your `Endpoint` module for pubsub then this is unnecessary. If you want to use
        a custom pub started with something like `{Phoenix.PubSub, name: MyName}`, then you can provide `MyName` to
        here.

        If this option is provided, we assume we are working with a `Phoenix.PubSub` and not a `Phoenix.Endpoint`, so
        the payload is sent as a `%Phoenix.Socket.Broadcast{}` if that module is available.
        """
      ]
    ]
  }

  @sections [@pub_sub]

  @moduledoc """
  A pubsub notifier extension
  """

  use Ash.Dsl.Extension, sections: @sections

  def publications(resource) do
    Ash.Dsl.Extension.get_entities(resource, [:pub_sub])
  end

  def module(resource) do
    Ash.Dsl.Extension.get_opt(resource, [:pub_sub], :module, nil)
  end

  def prefix(resource) do
    Ash.Dsl.Extension.get_opt(resource, [:pub_sub], :prefix, nil)
  end

  def name(resource) do
    Ash.Dsl.Extension.get_opt(resource, [:pub_sub], :name, nil)
  end

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
            [prefixed_topic, event, notification]

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
      %Phoenix.Socket.Broadcast{
        topic: topic,
        event: event,
        payload: notification
      }
    end
  else
    def to_payload(topic, event, notification) do
      %{
        topic: topic,
        event: event,
        payload: notification
      }
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
