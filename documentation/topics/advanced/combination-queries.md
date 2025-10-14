<!--
SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs.contributors>

SPDX-License-Identifier: MIT
-->

# Combination Queries

Ash Framework provides a powerful feature called "combination queries" that allows you to combine multiple queries into a single result set, giving you the ability to create complex data retrieval patterns with minimal effort. For SQL data-layers, this feature is implemented using SQL's UNION, INTERSECT, and EXCEPT operations.

## Overview

Combination queries let you:

- Combine multiple distinct queries into a single result set
- Apply different filters, sorting, limits, and calculations to each subquery
- Use operations like union, intersection, and exclusion to define how results should be combined
- Create complex composite queries that would otherwise require multiple separate database calls

## Syntax

To use combination queries, you'll work with the following functions:

```elixir
Ash.Query.combination_of(query, combinations)
```

Where `combinations` is a list of combination specifications starting with a base query, followed by additional operations:

- `Ash.Query.Combination.base/1`: The starting point for your combined query
- `Ash.Query.Combination.union/1`: Combine with the previous results, removing duplicates
- `Ash.Query.Combination.union_all/1`: Combine with the previous results, keeping duplicates
- `Ash.Query.Combination.intersect/1`: Keep only records that appear in both the previous results and this query
- `Ash.Query.Combination.except/1`: Remove records from the previous results that appear in this query

## Basic Example

Here's a simple example that combines users who meet different criteria:

```elixir
read :best_and_worst_users do
  description """
  Returns the top 10 active users who are not on a losing streak
  (sorted by score descending) and the bottom 10 active users who are not on a
  winning streak (sorted by score ascending)
  """

  filter expr(active == true)

  prepare fn query, _ ->
    Ash.Query.combination_of(query, [
      # Must always begin with a base combination
      Ash.Query.Combination.base(
        filter: expr(not(on_a_losing_streak)),
        sort: [score: :desc],
        limit: 10
      ),
      Ash.Query.Combination.union(
        filter: expr(not(on_a_winning_streak)),
        sort: [score: :asc],
        limit: 10
      )
    ])
  end
end
```

This query would return:
- The top 10 active users who are not on a losing streak (sorted by score descending)
- Union with the bottom 10 active users who are not on a winning streak (sorted by score ascending)

## Using Calculations in Combinations

One of the most powerful features of combination queries is the ability to create calculations that can be referenced across the combinations:

```elixir
query = "fred"

User
|> Ash.Query.filter(active == true)
|> Ash.Query.combination_of([
  Ash.Query.Combination.base(
    filter: expr(trigram_similarity(user_name, ^query) >= 0.5),
    calculations: %{
      match_score: calc(trigram_similarity(user_name, ^query), type: :float)
    },
    sort: [
      {calc(trigram_similarity(user_name, ^query), type: :float), :desc}
    ],
    limit: 10
  ),
  Ash.Query.Combination.union(
    filter: expr(trigram_similarity(email, ^query) >= 0.5),
    calculations: %{
      match_score: calc(trigram_similarity(email, ^query), type: :float)
    },
    sort: [
      {calc(trigram_similarity(email, ^query), type: :float), :desc}
    ],
    limit: 10
  )
])
|> Ash.read!()
```

When you add calculations to a combination query, they behave differently depending on the name of the calculation. If the name matches the name of an attribute, calculation or aggregate on the resource, then the value is placed in that key. Otherwise, it will be placed into the `calculations` key.

This example searches for users where either their name or email matches "fred" with a similarity score of at least 0.5, and returns the top 10 matches of each type sorted by their match score.

## Accessing Combination Values

To access values from combination queries in your main query, use the `combinations/1` function in your expressions:

```elixir
User
|> Ash.Query.combination_of([
  Ash.Query.Combination.base(
    filter: expr(organization.name == "bar"),
    calculations: %{
      domain: calc("bar", type: :string),
      full_name: calc(name <> "@bar", type: :string)
    }
  ),
  Ash.Query.Combination.union_all(
    filter: expr(organization.name == "baz"),
    calculations: %{
      domain: calc("baz", type: :string),
      full_name: calc(name <> "@baz", type: :string)
    }
  )
])
|> Ash.Query.calculate(:email_domain, :string, expr(^combinations(:domain)))
|> Ash.Query.calculate(:display_name, :string, expr(^combinations(:full_name)))
|> Ash.read!()
```

In this example, the `combinations(:domain)` and `combinations(:full_name)` references allow the outer query to access the calculation values from the inner combinations.

## Sorting and Distinct Operations

You can sort and filter the combined results using the calculations from your combinations:

```elixir
User
|> Ash.Query.combination_of([
  Ash.Query.Combination.base(calculations: %{sort_order: calc(3, type: :integer)}),
  Ash.Query.Combination.union_all(
    filter: expr(name == "alice"),
    calculations: %{sort_order: calc(1, type: :integer)}
  ),
  Ash.Query.Combination.union_all(
    filter: expr(name == "john"),
    calculations: %{sort_order: calc(2, type: :integer)}
  )
])
|> Ash.Query.sort([{calc(^combinations(:sort_order)), :asc}, {:name, :asc}])
|> Ash.Query.distinct(:name)
|> Ash.read!()
```

This will return results in the order: Alice, John, and then all other users, thanks to the custom sort_order calculation.

## Important Rules and Limitations

1. **This is an internal power tool**: No public interfaces like `AshJsonApi`/`AshGraphql` will be
    updated to allow this sort of query to be built "from the outside". It is designed to be implemented
    within an action, "under the hood".

2. **Base Combination Required**: Your list of combinations must always start with `Ash.Query.Combination.base/1`.

3. **Field Consistency**: All combinations must produce the same set of fields. This means:
   - If one combination has a calculation, all combinations need that calculation
   - Select statements should be consistent across combinations
   - If a calculation added to a combination has the same name as an attribute, then it will
     be used by `combinations(:that_field)`, allowing for combinations to "override" attribute
     values.

4. **Primary Keys**: When adding runtime calculations or loading related data with `Ash.Query.load/2`, all fieldsets must include the primary key of the resource. If this is not the case, the query will fail.

5. **Type Specification**: When referencing calculation values with `combinations/1`, the calculation must have been added with a specified type on the `base` query at a minimum:
   ```elixir
   # Correct - type is specified
   calc(expression, type: :string)

   # Incorrect - will raise an error when referenced
   calc(expression)
   ```

## Data Layer Support

Combination queries depend on data layer support. The implementation in this release includes support for ETS data layer, with implementation for SQL and Postgres to be added in future releases.

## Performance Considerations

Combination queries can be more efficient than multiple separate queries, especially when:

- You need to apply complex ordering or pagination to combined datasets
- You want to deduplicate results across multiple selection criteria
- You need to perform operations like intersection or exclusion between sets

However, be mindful that complex combinations can generate equally complex SQL queries, so monitor performance in production scenarios.

## Practical Examples

### Example 1: Search across multiple fields

```elixir
Post
|> Ash.Query.combination_of([
  Ash.Query.Combination.base(
    filter: expr(ilike(title, ^("%" <> search_term <> "%"))),
    calculations: %{match_type: calc("title", type: :string)},
    sort: [published_at: :desc],
    limit: 10
  ),
  Ash.Query.Combination.union(
    filter: expr(ilike(body, ^("%" <> search_term <> "%"))),
    calculations: %{match_type: calc("body", type: :string)},
    sort: [published_at: :desc],
    limit: 10
  )
])
|> Ash.Query.sort([published_at: :desc])
|> Ash.Query.calculate(:matched_in, :string, expr(^combinations(:match_type)))
|> Ash.read!()
```

### Example 2: Complex filtering with intersection

```elixir
User
|> Ash.Query.combination_of([
  Ash.Query.Combination.base(filter: expr(role == "admin")),
  Ash.Query.Combination.intersect(filter: expr(last_login > ^one_month_ago))
])
|> Ash.read!()
```

This returns all admin users who have logged in within the last month.

## Summary

Combination queries provide a powerful tool for creating complex data retrieval patterns in Ash. By combining multiple queries with different filters, sorts, and calculations, you can build sophisticated interfaces that would otherwise require multiple database queries and application-level merging of results.

This feature is particularly valuable for search interfaces, reporting tools, and anywhere you need to blend data from different filter conditions in a single, cohesive result set.
