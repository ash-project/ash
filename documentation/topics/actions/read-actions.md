<!--
SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs.contributors>

SPDX-License-Identifier: MIT
-->

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
- Run query preparations and validations (in definition order) | `d:Ash.Resource.Dsl.actions.read.prepare` and `d:Ash.Resource.Dsl.actions.read.validate`
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

## Customizing Queries When Calling Actions

When calling read actions through code interfaces, you can customize the query using the `query` option. This allows you to filter, sort, limit, and otherwise modify the results without manually building queries.

> #### User Input Safety {: .warning}
>
> When accepting query parameters from untrusted sources (like web requests), always use the `_input` variants (`sort_input`, `filter_input`) instead of the regular options.
> These functions only allow access to public fields and provide safe parsing of user input.

### Query Options via Code Interfaces

The `query` option accepts all the options that `Ash.Query.build/2` accepts:

```elixir
# Filtering results
posts = MyApp.Blog.list_posts!(
  query: [filter: [status: :published]]
)

# Sorting results
posts = MyApp.Blog.list_posts!(
  query: [sort: [published_at: :desc]]
)

# Limiting results
posts = MyApp.Blog.list_posts!(
  query: [limit: 10]
)

# Combining multiple query options
posts = MyApp.Blog.list_posts!(
  query: [
    filter: [status: :published, author_id: author.id],
    sort: [published_at: :desc],
    limit: 10,
    offset: 20
  ]
)

# Loading related data with query constraints
posts = MyApp.Blog.list_posts!(
  query: [
    load: [
      comments: [
        filter: [approved: true],
        sort: [created_at: :desc],
        limit: 5
      ]
    ]
  ]
)
```

### Handling User Input

When accepting query parameters from user input, use the safe input variants:

```elixir
# Safe sorting from user input
posts = MyApp.Blog.list_posts!(
  query: [sort_input: params["sort"] || "+published_at"]
)

# Safe filtering from user input
posts = MyApp.Blog.list_posts!(
  query: [filter_input: params["filter"] || %{}]
)

# Combining user input with application-defined constraints
posts = MyApp.Blog.list_posts!(
  query: [
    # User-controlled sorting
    sort_input: params["sort"],
    # User-controlled filtering
    filter_input: params["filter"],
    # Application-enforced constraints
    filter: [archived: false],
    limit: 100  # Prevent excessive data fetching
  ]
)
```

### Default Query Behavior in Actions

You can configure default query behavior in your action definitions:

```elixir
actions do
  read :recent_posts do
    # Default sort - overridden if user provides any sort
    prepare build(default_sort: [published_at: :desc])
    
    # Always applied filter - cannot be overridden
    filter expr(status == :published)
    
    # Default pagination
    pagination offset: true, default_limit: 20
  end
  
  read :search do
    argument :query, :string, allow_nil?: false
    
    # Prepare modifies the query before execution
    prepare fn query, _context ->
      Ash.Query.filter(query, contains(title, ^query.arguments.query))
    end
  end
  
  read :user_posts do
    argument :email, :string, allow_nil?: false
    argument :status, :string, default: "published"
    
    # Validate arguments before processing
    validate match(:email, ~r/^[^\s]+@[^\s]+\.[^\s]+$/) do
      message "must be a valid email address"
    end
    
    validate one_of(:status, ["published", "draft", "archived"])
    
    # Conditional validation - only validate if email is provided
    validate present(:email) do
      where present(:email)
    end
  end
end
```

### Building Queries Manually

For more complex scenarios, you can build queries manually before calling the action:

```elixir
require Ash.Query

# Build a complex query
query = 
  MyApp.Post
  |> Ash.Query.filter(status == :published)
  |> Ash.Query.sort(published_at: :desc)
  |> Ash.Query.limit(10)

# Execute the query
posts = Ash.read!(query)

# Or use it with a specific action
posts = Ash.read!(query, action: :published_posts)
```

### Common Query Patterns

#### Pagination

```elixir
# With page options
posts = MyApp.Blog.list_posts!(
  page: [limit: 20, offset: 40]
)

# with a query
MyApp.Post
|> Ash.Query.page(
  limit: 20, offset: 40
)

# when calling an action

MyApp.Post
|> Ash.Query.for_read(...)
|> Ash.read!(page: [limit: 20, offste: 40])
```

#### Complex Filtering

```elixir
# Filtering with relationships
posts = MyApp.Blog.list_posts!(
  query: [
    filter: [
      author: [verified: true],
      comments_count: [greater_than: 5]
    ]
  ]
)

# Using filter expressions (requires building query manually)
query = 
  MyApp.Post
  |> Ash.Query.filter(
    status == :published and 
    (author.verified == true or author.admin == true)
  )
```

## Validations on Read Actions

Read actions support validations to ensure query arguments meet your requirements before processing. Most built-in validations work on both changesets and queries.

Validations run alongside preparations during the query building phase, in the order they are defined in the action. This means you can mix preparations and validations, and they will execute in the sequence you specify.

### Supported Validations

The following built-in validations support queries:
- `action_is` - validates the action name
- `argument_does_not_equal`, `argument_equals`, `argument_in` - validates argument values
- `compare` - compares argument values 
- `confirm` - confirms two arguments match
- `match` - validates arguments against regex patterns
- `negate` - negates other validations
- `one_of` - validates arguments are in allowed values
- `present` - validates required arguments are present
- `string_length` - validates string argument length

### Validation Examples

```elixir
actions do
  read :user_search do
    argument :email, :string
    argument :role, :string
    argument :min_age, :integer
    argument :max_age, :integer
    
    # Validate email format
    validate match(:email, ~r/^[^\s]+@[^\s]+\.[^\s]+$/) do
      message "must be a valid email address"
    end
    
    # Validate role is one of allowed values
    validate one_of(:role, ["admin", "user", "moderator"])
    
    # Validate age range makes sense
    validate compare(:min_age, less_than: :max_age) do
      message "minimum age must be less than maximum age"
    end
    
    # Conditional validation - only validate email if provided
    validate present(:email) do
      where present(:email)
    end
    
    # Skip expensive validation if query is already invalid
    validate expensive_validation() do
      only_when_valid? true
    end
  end
end
```

### Where Clauses

Use `where` clauses to conditionally apply validations:

```elixir
read :conditional_search do
  argument :include_archived, :boolean, default: false
  argument :archive_reason, :string
  
  # Only validate archive_reason if including archived items
  validate present(:archive_reason) do
    where argument_equals(:include_archived, true)
  end
end
```

### only_when_valid? Option

Use `only_when_valid?` to skip validations when the query is already invalid:

```elixir
read :complex_search do
  argument :required_field, :string
  
  # This validation must pass
  validate present(:required_field)
  
  # This expensive validation only runs if query is valid so far
  validate expensive_external_validation() do
    only_when_valid? true
  end
end
```

For detailed information about query capabilities, see:
- `Ash.Query` module documentation for building queries
- `Ash.Query.build/2` for all available query options
- [Write Queries guide](/documentation/how-to/write-queries.livemd) for practical examples
- [Validations guide](/documentation/topics/resources/validations.md) for more validation examples
