defmodule Ash.Notifier.PubSub do
  @moduledoc "A pubsub notifier extension"

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
    modules: [:module],
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
      ]
    ]
  }

  use Ash.Dsl.Extension, sections: [@pub_sub]

  def publications(resource) do
    Ash.Dsl.Extension.get_entities(resource, [:pub_sub])
  end

  def module(resource) do
    Ash.Dsl.Extension.get_opt(resource, [:pub_sub], :module, nil)
  end

  def prefix(resource) do
    Ash.Dsl.Extension.get_opt(resource, [:pub_sub], :prefix, nil)
  end

  def notify(%Ash.Notifier.Notification{resource: resource} = notification) do
    resource
    |> publications()
    |> Enum.filter(&matches?(&1, notification))
    |> Enum.each(&publish_notification(&1, notification))
  end

  defp publish_notification(publish, notification) do
    publish.topic
    |> fill_template(notification)
    |> Enum.each(fn topic ->
      event = publish.event || to_string(notification.action.name)
      prefix = prefix(notification.resource) || ""
      prefixed_topic = prefix <> ":" <> topic

      module(notification.resource).broadcast(
        prefixed_topic,
        event,
        notification
      )
    end)
  end

  defp fill_template(topic, _) when is_binary(topic), do: [topic]

  defp fill_template(topic, %{action: %{type: type}, data: data})
       when type in [:create, :destroy] do
    topic
    |> Enum.map(fn item ->
      if is_binary(item) do
        item
      else
        data
        |> Map.get(item)
        |> to_string()
      end
    end)
    |> Enum.join(":")
    |> List.wrap()
  end

  defp fill_template(topic, notification) do
    topic
    |> all_combinations_of_values(notification)
    |> Enum.map(&List.flatten/1)
    |> Enum.map(&Enum.join(&1, ":"))
  end

  defp all_combinations_of_values(items, notification, trail \\ [])

  defp all_combinations_of_values([], _, trail), do: [Enum.reverse(trail)]

  defp all_combinations_of_values([item | rest], notification, trail) when is_binary(item) do
    all_combinations_of_values(rest, notification, [item | trail])
  end

  defp all_combinations_of_values([item | rest], notification, trail) when is_atom(item) do
    value_before_change = Map.get(notification.changeset.data, item)
    value_after_change = Map.get(notification.data, item)

    [value_before_change, value_after_change]
    |> Enum.reject(&is_nil/1)
    |> Enum.uniq()
    |> Enum.flat_map(fn possible_value ->
      all_combinations_of_values(rest, notification, [possible_value | trail])
    end)
  end

  defp matches?(%{action: action}, %{action: %{name: action}}), do: true
  defp matches?(%{type: type}, %{action: %{type: type}}), do: true

  defp matches?(_, _), do: false
end
