# Aggregates

Aggregates in Ash allow for retrieving summary information over groups of related data. A simple example might be to show the "count of published posts for a user".

## Declaring aggregates on a resource

Example:

```elixir
aggregates do
  count :count_of_posts, :posts, filter: [published: true]
end
```

See the documentation for the aggregates section in `Ash.Resource.Dsl.aggregates/1` for more information.

The aggregates declared on a resource allow for declaring a set of named aggregates that can be used by extensions.
They can also be loaded in the query using `Ash.Query.load/2`, or after the fact using `c:Ash.Api.load/2`. Aggregates declared on the resource will be keys in the resource's struct.

## Custom aggregates in the query

Example:

```elixir
User
|> Ash.Query.new()
|> Ash.Query.aggregate(:count_of_posts, :count, :posts, filter: [published: true])
```

See the documentation for `Ash.Query.aggregate/4` for more information.
