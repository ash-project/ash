defmodule Ash.Test.Notifier.BulkAwarePubSubTest do
  @moduledoc false
  use ExUnit.Case, async: true

  defmodule TestTopicGenerator do
    use Ash.Notifier.PubSub.TopicGenerator

    @impl true
    def required_loads do
      [author: :followers]
    end

    @impl true
    def generate_topics(notification, _opts) do
      case notification.action.name do
        :create ->
          followers = get_in(notification.data, [:author, :followers]) || []

          Enum.map(followers, fn follower ->
            "user_feed:#{follower.id}"
          end)

        _ ->
          []
      end
    end
  end

  defmodule BasicTopicGenerator do
    use Ash.Notifier.PubSub.TopicGenerator

    @impl true
    def generate_topics(notification, _opts) do
      ["basic:#{notification.data.id}"]
    end
  end

  test "topic generator with required_loads callback works" do
    assert function_exported?(TestTopicGenerator, :required_loads, 0)
    assert TestTopicGenerator.required_loads() == [author: :followers]
  end

  test "topic generator without required_loads callback works" do
    refute function_exported?(BasicTopicGenerator, :required_loads, 0)
  end

  test "Publication.topic accepts topic generators with required_loads" do
    assert {:ok, {TestTopicGenerator, []}} =
             Ash.Notifier.PubSub.Publication.topic(TestTopicGenerator)

    assert {:ok, {TestTopicGenerator, [foo: :bar]}} =
             Ash.Notifier.PubSub.Publication.topic({TestTopicGenerator, [foo: :bar]})
  end

  test "topic generators can declare required loads" do
    # Test the new simplified approach works
    assert function_exported?(TestTopicGenerator, :required_loads, 0)
    assert TestTopicGenerator.required_loads() == [author: :followers]
  end
end
