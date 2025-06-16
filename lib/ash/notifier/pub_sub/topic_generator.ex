defmodule Ash.Notifier.PubSub.TopicGenerator do
  @moduledoc """
  A behavior for implementing custom topic generation in PubSub notifications.

  This behavior allows you to implement custom logic for generating topics
  like broadcasting to all followers of a user or notifying related entities.

  ## Using the Macro

  You can use the `use` macro to automatically add the behavior:

  ```elixir
  defmodule MyApp.FollowerTopicGenerator do
    use Ash.Notifier.PubSub.TopicGenerator

    @impl true
    def generate_topics(notification, opts) do
      # Custom logic here
      ["topic1", "topic2"]
    end
  end
  ```

  ## Manual Behavior Implementation

  ```elixir
  defmodule MyApp.FollowerTopicGenerator do
    @behaviour Ash.Notifier.PubSub.TopicGenerator

    # Optional: Declare required loads for bulk optimization
    @impl true
    def required_loads do
      [author: :followers]
    end

    @impl true
    def generate_topics(notification, opts) do
      # Example: broadcast post creation to all followers
      # With required_loads/0, the data is already preloaded
      case notification.action.type do
        :create ->
          followers = get_in(notification.data, [:author, :followers]) || []

          Enum.map(followers, fn follower ->
            "user_feed:\#{follower.id}"
          end)

        _ ->
          []
      end
    end
  end
  ```

  ## Usage in Resources

  You can use the generator in several ways:

  ```elixir
  pub_sub do
    module MyEndpoint
    prefix "posts"

    # With options tuple
    publish_all :create, {MyApp.FollowerTopicGenerator, []}

    # Without options (empty list will be passed as opts)
    publish_all :update, MyApp.FollowerTopicGenerator
  end
  ```
  """
  @doc """
  Generate a list of topics based on the notification and options.

  ## Parameters

  - `notification` - The `%Ash.Notifier.Notification{}` struct containing
    the resource data, action, and other context
  - `opts` - A keyword list of options passed from the DSL configuration

  ## Returns

  Should return a list of strings representing the topics to publish to.
  An empty list means no topics will be published.
  """
  @callback generate_topics(notification :: Ash.Notifier.Notification.t(), opts :: keyword()) ::
              String.t()
              | [
                  String.t()
                ]

  @doc """
  Declare what relationships should be preloaded for efficient topic generation.

  This optional callback allows topic generators to specify relationship
  loading requirements. When implemented, the specified relationships will
  be automatically loaded before the notification is created, eliminating
  N+1 query problems in bulk operations.

  ## Returns

  Any load statement compatible with `Ash.load/2`:
  - `:relationship_name` - Load a single relationship
  - `[:rel1, :rel2]` - Load multiple relationships  
  - `[author: :followers]` - Load nested relationships
  - `[author: {:followers, query}]` - Load with custom query

  ## Examples

  ```elixir
  def required_loads do
    [author: :followers]
  end

  def required_loads do
    [:author, :tags, :comments]
  end
  ```
  """
  @callback required_loads() :: term()

  @optional_callbacks required_loads: 0

  @doc false
  defmacro __using__(_opts) do
    quote do
      @behaviour Ash.Notifier.PubSub.TopicGenerator
    end
  end
end
