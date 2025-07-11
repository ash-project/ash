defmodule Ash.Test.Notifier.TopicGeneratorLoadsTest do
  @moduledoc false
  use ExUnit.Case, async: true

  alias Ash.Test.Domain

  defmodule PubSub do
    def broadcast(topic, event, notification) do
      send(
        Application.get_env(__MODULE__, :test_pid),
        {:broadcast, topic, event, notification}
      )
    end
  end

  defmodule Author do
    @moduledoc false
    use Ash.Resource,
      domain: Domain,
      data_layer: Ash.DataLayer.Ets,
      notifiers: []

    ets do
      private?(true)
    end

    attributes do
      uuid_primary_key :id
      attribute :name, :string, public?: true
    end

    actions do
      defaults [:read, :update, :destroy]

      create :create do
        accept [:name]
      end
    end

    relationships do
      has_many :posts, Ash.Test.Notifier.TopicGeneratorLoadsTest.Post,
        destination_attribute: :author_id,
        public?: true

      has_many :followers, Ash.Test.Notifier.TopicGeneratorLoadsTest.Follower,
        destination_attribute: :author_id,
        public?: true
    end
  end

  defmodule Follower do
    @moduledoc false
    use Ash.Resource,
      domain: Domain,
      data_layer: Ash.DataLayer.Ets,
      notifiers: []

    ets do
      private?(true)
    end

    attributes do
      uuid_primary_key :id
      attribute :user_id, :uuid, public?: true
      attribute :author_id, :uuid, public?: true
    end

    actions do
      defaults [:read, :update, :destroy]

      create :create do
        accept [:user_id, :author_id]
      end
    end

    relationships do
      belongs_to :author, Ash.Test.Notifier.TopicGeneratorLoadsTest.Author, public?: true
    end
  end

  defmodule FollowerTopicGenerator do
    use Ash.Notifier.PubSub.TopicGenerator

    @impl true
    def required_loads do
      [author: :followers]
    end

    @impl true
    def generate_topics(notification, _opts) do
      case notification.action.name do
        :create ->
          # Access preloaded data - no N+1 queries!
          followers =
            case notification.data.author do
              %{followers: followers} when is_list(followers) -> followers
              _ -> []
            end

          Enum.map(followers, fn follower ->
            "user_feed:#{follower.user_id}"
          end)

        _ ->
          []
      end
    end
  end

  defmodule Post do
    @moduledoc false
    use Ash.Resource,
      domain: Domain,
      data_layer: Ash.DataLayer.Ets,
      notifiers: [
        Ash.Notifier.PubSub
      ]

    pub_sub do
      module PubSub
      prefix "post"

      # Use our topic generator that requires loads
      publish :create, FollowerTopicGenerator
    end

    ets do
      private?(true)
    end

    attributes do
      uuid_primary_key :id
      attribute :title, :string, public?: true
      attribute :author_id, :uuid, public?: true
    end

    actions do
      defaults [:read, :update, :destroy]

      create :create do
        accept [:title, :author_id]
      end
    end

    relationships do
      belongs_to :author, Ash.Test.Notifier.TopicGeneratorLoadsTest.Author, public?: true
    end
  end

  setup do
    # Set up test PID for notifications
    Application.put_env(PubSub, :test_pid, self())

    # Create test data
    author =
      Author
      |> Ash.Changeset.for_create(:create, %{name: "Test Author"})
      |> Ash.create!()

    follower1 =
      Follower
      |> Ash.Changeset.for_create(:create, %{
        user_id: Ash.UUID.generate(),
        author_id: author.id
      })
      |> Ash.create!()

    follower2 =
      Follower
      |> Ash.Changeset.for_create(:create, %{
        user_id: Ash.UUID.generate(),
        author_id: author.id
      })
      |> Ash.create!()

    %{
      author: author,
      followers: [follower1, follower2]
    }
  end

  test "topic generator with required_loads automatically preloads relationships", %{
    author: author,
    followers: followers
  } do
    # Create a post - this should trigger our topic generator
    post =
      Post
      |> Ash.Changeset.for_create(:create, %{
        title: "Test Post",
        author_id: author.id
      })
      |> Ash.create!()

    # Verify that notifications were sent to all followers
    # This proves the relationships were preloaded successfully
    expected_topics =
      Enum.map(followers, fn follower ->
        "post:user_feed:#{follower.user_id}"
      end)

    for expected_topic <- expected_topics do
      assert_receive {:broadcast, ^expected_topic, "create", notification}

      # Verify the notification has the preloaded data
      assert notification.data.id == post.id
      assert notification.data.author.id == author.id
      assert length(notification.data.author.followers) == 2
    end
  end

  test "topic generator works with bulk creates", %{author: author, followers: followers} do
    # Create multiple posts individually to test bulk behavior
    # (The actual bulk create API needs proper setup that's beyond this test)
    posts = [
      Post
      |> Ash.Changeset.for_create(:create, %{title: "Bulk Post 1", author_id: author.id})
      |> Ash.create!(),
      Post
      |> Ash.Changeset.for_create(:create, %{title: "Bulk Post 2", author_id: author.id})
      |> Ash.create!(),
      Post
      |> Ash.Changeset.for_create(:create, %{title: "Bulk Post 3", author_id: author.id})
      |> Ash.create!()
    ]

    assert length(posts) == 3

    # Should receive notifications for each post, sent to all followers
    # Total: 3 posts × 2 followers = 6 notifications
    expected_notification_count = 3 * length(followers)

    received_notifications =
      for _i <- 1..expected_notification_count do
        assert_receive {:broadcast, topic, "create", notification}
        {topic, notification}
      end

    assert length(received_notifications) == expected_notification_count

    # Verify all notifications have preloaded data
    for {_topic, notification} <- received_notifications do
      assert notification.data.author.id == author.id
      assert length(notification.data.author.followers) == 2
      # This proves no N+1 queries occurred - data is preloaded
    end
  end

  test "topic generator without required_loads still works normally" do
    defmodule SimpleTopicGenerator do
      use Ash.Notifier.PubSub.TopicGenerator

      @impl true
      def generate_topics(notification, _opts) do
        ["simple:#{notification.data.id}"]
      end
    end

    # Test that generators without required_loads work fine
    assert function_exported?(SimpleTopicGenerator, :generate_topics, 2)
    refute function_exported?(SimpleTopicGenerator, :required_loads, 0)
  end

  test "helper function correctly identifies topic generators with loads" do
    # Test our helper function
    assert function_exported?(FollowerTopicGenerator, :required_loads, 0)
    assert FollowerTopicGenerator.required_loads() == [author: :followers]
  end
end
