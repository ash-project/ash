<!--
SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs.contributors>

SPDX-License-Identifier: MIT
-->

# Aggregates

Aggregates in Ash allow for retrieving summary information over groups of related data. A simple example might be to show the "count of published posts for a user". Aggregates allow us quick and performant access to this data, in a way that supports being filtered/sorted on automatically. More aggregate types can be added, but you will be restricted to only the supported types. In cases where aggregates don't suffice, use [Calculations](/documentation/topics/resources/calculations.md), which are intended to be much more flexible.

Aggregates can work in two ways:
1. **Relationship-based aggregates** - aggregate over data through relationships (the traditional approach)
2. **Resource-based aggregates** - aggregate over any resource directly without requiring a relationship

## Declaring aggregates on a resource

Aggregates are defined in an `aggregates` section. For all possible types, see below.
For a full reference, see `d:Ash.Resource.Dsl.aggregates`.

### Relationship-based Aggregates

```elixir
aggregates do
  count :count_of_posts, :posts do
    filter expr(published == true)
  end
end
```

### Resource-based Aggregates

Resource-based aggregates allow you to aggregate over any resource without needing a relationship. Instead of providing a relationship path, you provide the target resource module directly:

```elixir
aggregates do
  # Count profiles with matching name
  count :matching_profiles_count, Profile do
    filter expr(name == parent(name))
  end
  
  # Sum scores from reports where author matches user's name
  sum :total_report_score, Report, :score do
    filter expr(author_name == parent(name))
  end
  
  # Check if any active profile exists (no parent filter needed)
  exists :has_active_profile, Profile do
    filter expr(active == true)
  end
end
```

The `parent/1` function allows you to reference fields from the source resource within the aggregate's filter expression.

## Using an aggregate

Aggregates are loaded and filtered on in the same way that calculations are. Lets look at some examples:

### Loading aggregates in a query or on records

```elixir
User
|> Ash.Query.load(:count_of_posts)
# => %Ash.Query{aggregates: %{count_of_posts: %Ash.Query.Aggregate{...}}, ...}

users
|> Ash.load!(:count_of_posts)
|> Enum.map(&(&1.count_of_posts))
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

## Custom Aggregate Example: Percentile

Here's an example of creating a custom aggregate that uses PostgreSQL's `PERCENTILE_CONT` function to calculate percentiles:

```elixir
defmodule PercentileAggregate do
  @moduledoc false
  use AshPostgres.CustomAggregate

  require Ecto.Query

  @impl true
  def dynamic(opts, binding) do
    Ecto.Query.dynamic(
      [],
      fragment(
        "PERCENTILE_CONT(?) WITHIN GROUP (ORDER BY ?)",
        ^opts[:percentile],
        field(as(^binding), ^opts[:field])
      )
    )
  end
end
```

### Usage in Resource

```elixir
aggregates do
  custom :median_daily_distance_walked,
         :daily_walks,
         :integer,
         implementation: {
           PercentileAggregate,
           field: :distance_walked_meters,
           percentile: 0.5
         },
         filter: expr(distance_walked_meters > 0)
end
```

This aggregate calculates the median (50th percentile) distance walked by cats each day, filtering out any days where no walking occurred.

### Relationship-based aggregate example:

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

### Resource-based aggregate example:

```elixir
User
|> Ash.Query.aggregate(
  :matching_profiles,
  :count,
  Profile,
  query: [
    filter: expr(name == parent(name))
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

### Relationship-based inline aggregates
```elixir
calculate :grade, :decimal, expr(
  count(answers, query: [filter: expr(correct == true)]) /
  count(answers, query: [filter: expr(correct == false)])
)
```

### Resource-based inline aggregates
```elixir
calculate :profile_summary, :map, expr(%{
  matching_profiles: count(Profile, filter: expr(name == parent(name))),
  total_reports: count(Report, filter: expr(author_name == parent(name)))
})
```

See the [Expressions guide](/documentation/topics/reference/expressions.md#inline-aggregates) for more.
