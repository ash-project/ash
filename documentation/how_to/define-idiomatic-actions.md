# Define Idiomatic Actions

The best practice is typically to try to push things as far down into your resources as possible. 

## The Non-idiomatic Way

If you were doing a twitter front page, you might have a tweet resource with a simple action like this:

```elixir
# use a simple primary read
defaults [:read, ...]
```

And in doing that, you could get all the tweets with something like this:

```elixir
Tweet
|> Ash.Query.for_read(:read)
|> Ash.Query.sort(posted_at: :desc)
|> Ash.Query.filter(author.id == ^current_user.id or exists(author.friends, id == ^current_user.id))
# assuming the name of your api was `Tweets`
|> Tweets.read!()
```

And that works and in some cases might be the right way to do what you're trying to do
And lets say there was a sort drop down that made it sort by popular instead of recent, you could do something like this:

```elixir
Tweet
|> Ash.Query.for_read(:read)
|> then(fn query -> 
  case sort do
    :recent ->
      Ash.Query.sort(query, posted_at: :desc)
    :popular ->
      Ash.Query.sort(query, like_count: :desc)
  end
end)
|> Ash.Query.filter(author.id == ^current_user.id or exists(author.friends, id == ^current_user.id))
# assuming the name of your api was `Tweets`
|> Tweets.read!()
```

## The Idiomatic Way

But the better way to model this would be something like this:

```elixir
code_interface do
  define_for MyApp.Tweets
  define :front_page, args: [:sort_by]
end
  
read :front_page do
  argument :sort_by, :atom do
    constraints one_of: [:recent, :popular]
  end

  prepare MyApp.Tweets.Tweet.Preparations.SortFrontPage
  filter expr(author.id == ^actor(:id) or exists(author.friends, id == ^actor(:id))
end
```

Custom preparations allow you to do all sorts of things, in this case handle custom sorting

```elixir
defmodule MyApp.Tweets.Tweet.Preparations.SortFrontPage do
  use Ash.Resource.Preparation

  def prepare(query, _, _) do
    # We use `prepend?` to put the sort ahead of any other specified sort on the query
    case Ash.Changeset.get_argument(query, :sort_by) do
      :recent ->
        Ash.Query.sort(query, [posted_at: :desc], prepend?: true)

      :popular ->
        Ash.Query.sort(query, [like_count: :desc], prepend?: true)
    end
  end
end
```

And then you can get the front page of tweets far more cleanly:

```elixir
Tweet.front_page!(socket.assigns.sort_by)
```

If you were using AshGraphql, you could do something like this:

```elixir
graphql do
  type :tweet

  queries do
    query :front_page, :front_page
  end
end
```

And because you've made the action encompass the entire logic of fetching the front page, you've got automatic support for API access to your system.
This is just one example of the benefits of having idiomatic and complete actions.