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

    @impl true
    def generate_topics(notification, opts) do
      # Example: broadcast post creation to all followers
      case notification.action.type do
        :create ->
          followers =
            notification.data
            |> Ash.load!([author: :followers])
            |> Map.get(:author, %{})
            |> Map.get(:followers, [])

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

  ## Examples

  ```elixir
  def generate_topics(notification, _opts) do
    case notification.data do
      %{author_id: author_id} when not is_nil(author_id) ->
        ["author_feed:\#{author_id}"]
      _ ->
        []
    end
  end
  ```
  """
  @callback generate_topics(notification :: Ash.Notifier.Notification.t(), opts :: keyword()) ::
              String.t()
              | [
                  String.t()
                ]

  @doc false
  defmacro __using__(_opts) do
    quote do
      @behaviour Ash.Notifier.PubSub.TopicGenerator
    end
  end
end
