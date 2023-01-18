# Aggregates

Aggregates in Ash allow for retrieving summary information over groups of related data. A simple example might be to show the "count of published posts for a user". Aggregates allow us quick and performant access to this data, in a way that supports being filtered/sorted on automatically. More aggregate types can be added, but you will be restricted to only the supported types. In cases where aggregates don't suffice, use [Calculations](/documentation/topics/calculations.md), which are intended to be much more flexible.

## Declaring aggregates on a resource

Example:

```elixir
aggregates do
  count :count_of_posts, :posts do
    filter expr(published == true)
  end
end
```

The available aggregate types are:

- `count` - counts related items meeting the criteria
- `first` - gets the first related value matching the criteria. Must specify the `field` to get.
- `sum` - sums the related items meeting the criteria. Must specify the `field` to sum.
- `list` - lists the related values. Must specify the `field` to list.

See the docs on `d:Ash.Resource.aggregates` for more information.

The aggregates declared on a resource allow for declaring a set of named aggregates that can be used by extensions.

As an escape hatch, they can also be loaded in the query using `Ash.Query.load/2`, or after the fact using `c:Ash.Api.load/3`. Aggregates declared on the resource will be keys in the resource's struct.

## Custom aggregates in the query

Custom aggregates can be added to the query and will be placed in the `aggregates` key of the results. This is an escape hatch, and is not the primary way that you should be using aggregates. It does, however, allow for dynamism, i.e if you are accepting user input that determines what the filter and/or field should be, that kind of thing.

Example:

```elixir
User
|> Ash.Query.new()
|> Ash.Query.aggregate(:count_of_posts, :count, :posts, filter: [published: published?])
```

See the documentation for `Ash.Query.aggregate/4` for more information.
