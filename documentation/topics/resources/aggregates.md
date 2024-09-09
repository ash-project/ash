# Aggregates

Aggregates in Ash allow for retrieving summary information over groups of related data. A simple example might be to show the "count of published posts for a user". Aggregates allow us quick and performant access to this data, in a way that supports being filtered/sorted on automatically. More aggregate types can be added, but you will be restricted to only the supported types. In cases where aggregates don't suffice, use [Calculations](/documentation/topics/resources/calculations.md), which are intended to be much more flexible.

## Declaring aggregates on a resource

Aggregates are defined in an `aggregates` section. For all possible types, see below.
For a full reference, see `d:Ash.Resource.Dsl.aggregates`.

```elixir
aggregates do
  count :count_of_posts, :posts do
    filter expr(published == true)
  end
end
```

## Using an aggregate

Aggregates are loaded and filtered on in the same way that calculations are. Lets look at some examples:

### Loading aggregates in a query or on records

```elixir
User
|> Ash.Query.load(:count_of_posts)
|> Map.get(:count_of_posts)
# => 10

users
|> Ash.load!(:count_of_posts)
|> Enum.map(&(&1.count_of_posts)
# => [3, 5, 2]
```

### Filtering on aggregates

```elixir
require Ash.Query

User
|> Ash.Query.filter(count_of_posts > 10)
|> Ash.read!()
```

### Sorting aggregates

```elixir
User
|> Ash.Query.sort(count_of_posts: :asc)
|> Ash.read!()
```

## Aggregate types

- `count` - counts related items meeting the criteria.
- `exists` - checks if any related items meet the criteria.
- `first` - gets the first related value matching the criteria. Must specify the `field`.
- `sum` - sums the related items meeting the criteria. Must specify the `field`.
- `list` - lists the related values. Must specify the `field`.
- `max` - gets the maximum related value. Must specify the `field`.
- `min` - gets the minimum related value. Must specify the `field`.
- `avg` - gets the average related value. Must specify the `field`.
- `custom` - allows for a custom aggregate. Implementation depends on the data layer. Must provide an `implementation`.

The declared set of named aggregates can be used by extensions and referred to throughout your application As an escape hatch, they can also be loaded in the query using `Ash.Query.load/2`, or after the fact using `Ash.load/3`. Aggregates declared on the resource will be keys in the resource's struct.

See the docs on `d:Ash.Resource.Dsl.aggregates` for more information.

## Custom aggregates in the query

Custom aggregates can be added to the query and will be placed in the `aggregates` key of the results. This is an escape hatch, and is not the primary way that you should be using aggregates. It does, however, allow for dynamism, i.e if you are accepting user input that determines what the filter and/or field should be, that kind of thing.

Example:

```elixir
User
|> Ash.Query.aggregate(
  :count_of_posts,
  :count,
  :posts,
  query: [
    filter: [published: published?]
  ]
)
```

See the documentation for `Ash.Query.aggregate/4` for more information.

## Join Filters

Join filters allows for more complex aggregate queries, including joining with predicates based on multiple related values.

### Example

```elixir
  aggregates do
    sum :saved_money, [:redeems, :deal], :amount do
      # where any redeem of the deal is redeemed
      filter expr(redeems.redeemed == true)

      # where the `redeems` are `redeemed`
      join_filter :redeems, expr(redeemed == true)

      # where the `redeems.deal.active` == `redeems.require_active`
      join_filter [:redeems, :deal], expr(active == parent(require_active))
    end
  end
```

## Inline Aggregates

Aggregates can be created in-line in expressions, with their relationship path specified and any options provided that match the options given to `Ash.Query.Aggregate.new/4`. For example:

```elixir
calculate :grade, :decimal, expr(
  count(answers, query: [filter: expr(correct == true)]) /
  count(answers, query: [filter: expr(correct == false)])
)
```

See the [Expressions guide](/documentation/topics/reference/expressions.md#inline-aggregates) for more.
