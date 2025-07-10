# Read Actions

Read actions operate on an `Ash.Query`. Read actions always return lists of data. The act of [pagination](#pagination), or returning a [single result](#ash-get), is handled as part of the interface, and is not a concern of the action itself. Here is an example of a read action:

```elixir
# Giving your actions informative names is always a good idea
read :ticket_queue do
  # Use arguments to take in values you need to run your read action.
  argument :priorities, {:array, :atom} do
    constraints items: [one_of: [:low, :medium, :high]]
  end

  # This action may be paginated,
  # and returns a total count of records by default
  pagination offset: true, countable: :by_default

  # Arguments can be used in preparations and filters
  filter expr(status == :open and priority in ^arg(:priorities))
end
```

For a full list of all of the available options for configuring read actions, see [the Ash.Resource.Dsl documentation](dsl-ash-resource.html#actions-read).

## Calling Read Actions

The basic formula for calling a read action looks like this:

```elixir
Resource
|> Ash.Query.for_read(:action_name, %{argument: :value}, ...opts)
|> Ash.read!()
```

See below for variations on action calling, and see the [code interface guide](/documentation/topics/code-interfaces.md) guide for how to
define idiomatic and convenient functions that call your actions.

## Ash.get!

The `Ash.get!` function is a convenience function for running a read action, filtering by a unique identifier, and expecting only a single result. It is equivalent to the following code:

```elixir
# action can be omitted to use the primary read action
Ash.get!(Resource, 1, action: :read_action)

# is roughly equivalent to

Resource
|> Ash.Query.filter(id == 1)
|> Ash.Query.limit(2)
|> Ash.Query.for_read(:read_action, %{})
|> Ash.read!()
|> case do
  [] -> # raise not found error
  [result] -> result
  [_, _] -> # raise too many results error
end
```

## Ash.read_one!

The `Ash.read_one!` function is a similar convenience function to `Ash.get!`, but it does not take a unique identifier. It is useful when you expect an action to return only a single result, and want to enforce that and return a single result.

```elixir
Ash.read_one!(query)

# is roughly equivalent to

query
|> Ash.Query.limit(2)
|> Ash.read!()
|> case do
  [] -> nil
  [result] -> result
  [_, _] -> # raise too many results error
end
```

## Pagination

Ash provides built-in support for pagination when reading resources and their relationships. You can find more information about this in the [pagination guide](/documentation/topics/advanced/pagination.livemd).

> ### Pagination configuration on default vs custom read actions {: .info}
>
> The default read action supports keyset pagination automatically. You need to explicitly enable pagination strategies you want to support when defining your own read actions.

## What happens when you call Ash.Query.for_read/4

The following steps are performed when you call `Ash.Query.for_read/4`.

- Cast input arguments - `d:Ash.Resource.Dsl.actions.read.argument`
- Set default argument values - `d:Ash.Resource.Dsl.actions.read.argument|default`
- Add errors for missing required arguments | `d:Ash.Resource.Dsl.actions.read.argument|allow_nil?`
- Run query preparations | `d:Ash.Resource.Dsl.actions.read.prepare`
- Add action filter | `d:Ash.Resource.Dsl.actions.read|filter`

## What happens when you run the action

These steps are trimmed down, and are aimed at helping users understand the general flow. Some steps are omitted.

- Run `Ash.Query.for_read/3` if it has not already been run
- [Apply tenant filters for attribute](/documentation/topics/advanced/multitenancy.md)
- Apply [pagination](/documentation/topics/advanced/pagination.livemd) options
- Run before action hooks
- Multi-datalayer filter is synthesized. We run queries in other data layers to fetch ids and translate related filters to `(destination_field in ^ids)`
- Strict Check & Filter Authorization is run
- Data layer query is built and validated
- Field policies are added to the query
- Data layer query is Run
- Authorizer "runtime" checks are run (you likely do not have any of these)

The following steps happen while(asynchronously) or after the main data layer query has been run

- If paginating and count was requested, the count is determined at the same time as the query is run.
- Any calculations & aggregates that were able to be run outside of the main query are run
- Relationships, calculations, and aggregates are loaded

## Sorting Results

Ash provides several ways to sort the results of read actions. Sorting can be applied at different levels: when calling actions, as default sorting in action definitions, or dynamically through queries.

> #### Sorting User Input {: .warning}
>
> When accepting sort parameters from untrusted sources (like web requests), always use `sort_input` instead of `sort`.
> See [Sorting from User Input](#sorting-from-user-input) below for details.

### Sorting via Code Interfaces

The most common way to sort results is using the `sort` option within the `query` parameter when calling actions through code interfaces:

```elixir
# Simple sorting by a single field
posts = MyApp.Blog.list_posts!(
  query: [sort: [published_at: :desc]]
)

# Sorting by multiple fields
posts = MyApp.Blog.list_posts!(
  query: [sort: [status: :asc, published_at: :desc]]
)

# Combining sort with other query options
posts = MyApp.Blog.list_posts!(
  query: [
    filter: [status: :published],
    sort: [published_at: :desc],
    limit: 10
  ]
)
```

### Default Sorting in Actions

You can set default sorting for read actions using `prepare build(default_sort: ...)`:

```elixir
actions do
  read :recent_posts do
    # Default sort by published_at descending
    # This sort is ignored if any sort is provided when calling the action
    prepare build(default_sort: [published_at: :desc])
  end
  
  read :top_posts do
    # Default sort by score, then by published date
    prepare build(default_sort: [score: :desc, published_at: :desc])
  end
end
```

Note: If you use `prepare build(sort: ...)` instead, any sort provided when calling the action will be **appended** to the prepared sort, not replace it. Choose `build(default_sort: ...)` when you want the sort to be overridable, and `build(sort: ...)` when you want to enforce a primary sort order.

### Sorting with Query Building

When building queries manually, use `Ash.Query.sort/2`:

```elixir
require Ash.Query

MyApp.Post
|> Ash.Query.sort(published_at: :desc)
|> Ash.read!()

# Multiple sort fields
MyApp.Post
|> Ash.Query.sort([{:priority, :desc}, {:created_at, :asc}])
|> Ash.read!()
```

### Sorting from User Input

When accepting sort parameters from user input (like from a web request), use `Ash.Query.sort_input/2` or the `sort_input` option in code interfaces:

```elixir
# Using sort_input in code interfaces (preferred)
posts = MyApp.Blog.list_posts!(
  query: [sort_input: params["sort"] || "+published_at"]
)

# Parse string-based sort input directly with Ash.Query
MyApp.Post
|> Ash.Query.sort_input("+published_at,-title")
|> Ash.read!()
```

The `sort_input` function safely parses user input and only allows sorting on public fields. It supports various formats:
- String format: `"+field1,-field2"` (+ for ascending, - for descending)
- List format: `["field1", "-field2"]`
- Keyword format: `[field1: :asc, field2: :desc]`

For more information about input parsing and validation, see the [Write Queries guide](/documentation/how-to/write-queries.livemd#sorting).

### Tips for Using Sort

1. **Default vs prepared sorts**: Use `build(default_sort: ...)` for sorts that users can override, and `build(sort: ...)` for sorts that should always be applied (with user sorts appended).

2. **Sorting and pagination**: When using keyset pagination, ensure your sort includes a unique field (like the primary key) to guarantee stable pagination.

3. **Performance**: Sorting by attributes is generally more efficient than sorting by calculations or aggregates. Consider adding database indexes for frequently sorted fields.

For detailed information about sorting capabilities including sort orders, expressions, and calculations, see the `Ash.Query.sort/2` documentation.
