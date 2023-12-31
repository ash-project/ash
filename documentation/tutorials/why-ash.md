# Why Ash?

One of the fundamental ideas behind Ash is that when the various components of your system can have consistent expectations of how the other components around them work, you can ultimately do a significant amount more, with less.

## Example: Policies

Policy authorization is a good example of this. When you're trying to write a flexible system that has secure access patterns, there are tons of ways to do it wrong. For example, lets say you have this policy on your "posts" resource.

```elixir
policy action_type(:read) do
  authorize_if relates_to_actor_via(:author)
  authorize_if relates_to_actor_via([:author, :friends])
end
```

Using Ash, if I were to make a simple query like MyApp.Blog.read!(MyApp.Blog.Post, actor: current_user), what it would do is automatically translate those policies to a filter statement. So it is the equivalent to saying:

```elixir
MyApp.Blog.Post
|> Ash.Query.filter(exists(author, id == ^current_user.id) or exists(author.friends, id == ^current_user.id))
|> MyApp.Blog.read!()
```

And that is something you could potentially hand roll. But what about when you want to say something like this:

```elixir
MyApp.Blog.Comment
|> Ash.Query.filter(exists(author, email == "daarb@daarb.com"))
|> MyApp.Blog.read!()
```

Ash is aware of the policies on author and will translate this under the hood to

```elixir
MyApp.Blog.Comment
|> Ash.Query.filter(exists(author, id == ^current_user.id and #Ash.Filter<policies for reading authors>))
```

Or if you want to display aggregate information, i.e in Ash

```elixir
# on an Organization resource
aggregates do
  count :count_of_posts, :posts
end
```

That should realistically show "the number of posts the user can see" (by default). So Ash is aware of the policies and details of the resource you are aggregating, meaning that aggregate will just "do the right thing"

Policies are just one example of how a tool that is built for this kind of thing can often come with features that would be entirely unreasonable for developers to write by hand for every platform they are building. The declarative design patterns behind Ash allow us to build features that are context aware and extremely powerful.

## It isn't about "less code"

In all reality "writing less code" is not really a goal that Ash has. The real goal behind helping people to not reinvent the wheel is that if we keep reinventing the wheel every time, the wheel will never be properly iterated on.
Policies are one example of this.

By ensuring that the various pieces of our system have consistent, stable and rich interfaces, we can easily write reusable extensions. For example, `AshArchival` will ensure that any given resource is "soft deleted" instead of actually deleted, and `AshPaperTrail` will keep a log of all changes that happen on a given resource. Both of these behaviors can be introduced to your resources with a single line of code, e.g `extensions: [AshPaperTrail.Resource]` because resources are composed of elements that have consistent, *declarative* structures.
