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

## Ash.get!

The `Ash.get!` function is a convenience function for running a read action, filtering by a unique identifier, and expecting only a single result. It is equivalent to the following code:

```elixir
Ash.get!(Resource, 1)

# is roughly equivalent to

Resource
|> Ash.Query.filter(id == 1)
|> Ash.Query.limit(2)
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

Pagination when reading records is configured on a per-action basis. Ash supports two kinds of pagination: `keyset` and `offset`.

A single action can use both kinds of pagination if desired, but typically you would use one or the other.

For pagination configuration reference, see `d:Ash.Resource.Dsl.actions.read.pagination`.

> #### Counting records {: .tip}
>
> When calling an action that uses pagination, the full count of records can be requested by adding the option `page: [count: true]`.
> Note that this will perform a similar query a second time to fetch the count, which can be expensive on large data sets.

### Offset Pagination

Offset pagination is done via providing a `limit` and an `offset` when making queries.

* The `limit` determines how many records should be returned in the query.
* The `offset` describes how many records from the beginning should be skipped.

Using this, you might make requests like the following:

```elixir
# Get the first ten records
Ash.read(Resource, page: [limit: 10])
# or by using an action named `read` directly through a
# code interface on the domain
Domain.read(page: [limit: 10])

# Get the next ten records
Ash.read(Resource, page: [limit: 10, offset: 10])
# or by using an action named `read` directly through a
# code interface on the domain
Domain.read(page: [limit: 10, offset: 10])
```

Next/previous page requests can also be made in memory, using an existing page of search results:

```elixir
# Return page three of search results
{:ok, third_page} = Resource.read(page: [limit: 10, offset: 20])

# Use `:prev` and `:next` to go backwards and forwards.
# `:first`, `:last`, `:self` and specifying a page number are also supported.
{:ok, second_page} = Ash.page(third_page, :prev)
{:ok, fourth_page} = Ash.page(third_page, :next)
```

#### Pros of offset pagination

- Simple to think about
- Possible to skip to a page by number. E.g the 5th page of 10 records is `offset: 40`
- Easy to reason about what page you are currently on (if the total number of records is requested)
- Can go to the last page (though data may have changed between calculating the last page details, and requesting it)

#### Cons of offset pagination

- Does not perform well on large datasets (if you have to ask if your dataset is "large", it probably isn't)
- When moving between pages, if data was created or deleted, individual records may be missing or appear on multiple pages

### Keyset Pagination

Keyset pagination is done via providing an `after` or `before` option, as well as a `limit`.

* The `limit` determines how many records should be returned in the query.
* The `after` or `before` value should be a `keyset` value that has been returned from a previous request. Keyset values are returned whenever there is any read action on a resource that supports keyset pagination, and they are stored in the `__metadata__` key of each record.


> #### Keysets are directly tied to the sorting applied to the query {: .warning}
>
>  You can't change the sort applied to a request being paginated, and use the same keyset. If you want to change the sort, but *keep* the record who's keyset you are using in the `before` or `after` option,  you must first request the individual record, with the new sort applied. Then, you can use the new keyset.


For example:

```elixir
{:ok, page} = Ash.read(Resource, page: [limit: 10])
# Returns `{:ok, %Ash.Page.Keyset{results: [...], before: nil, after: nil}}`
# The `before`/`after` values are the keysets used for this request.

# Fetch the keyset for the next request from the results list
last_record = List.last(page.results)
# Returns `%Resource{__metadata__: %{keyset: "g2wAAAABbQAAACQzOWNjNTcwNy00NjlmL..."}, ...}``

# Use this keyset value to fetch the next page
{:ok, next_page} = Ash.read(Resource, page: [limit: 10, after: last_record.__metadata__.keyset])
```

Like offset pagination, next/previous page requests can also be made in memory, using an existing page of search results:

```elixir
# Return page three of search results
{:ok, third_page} = Resource.read(page: [limit: 10])

# Use `:prev` and `:next` to go backwards and forwards.
# `:first` and `:self` can also be used, but `:last` and specifying a page number are not supported.
{:ok, second_page} = Ash.page(third_page, :prev)
{:ok, fourth_page} = Ash.page(third_page, :next)
```

#### Pros of keyset pagination

- Performs very well on large datasets (assuming indices exist on the columns being sorted on)
- Behaves well as data changes. The record specified will always be the first or last item in the page

#### Cons of keyset paginations

- A bit more complex to use
- Can't go to a specific page number

### Example implementation

#### Setting up the resource

Add the `pagination` macro call to the [action](/documentation/topics/reference/glossary.md#action) of the [resource](/documentation/topics/reference/glossary.md#resource) that you want to be paginated.

```elixir
defmodule AppName.ResourceName do
  use Ash.Resource

  actions do
    read :read_action_name do
      pagination offset?: true, default_limit: 3, countable: true
    end

    # ...
```

For all available pagination options, see `d:Ash.Resource.Dsl.actions.read|pagination`.

> #### Check the updated query return type! {: .info}
> Pagination will modify the return type of calling the query action.
>
> Without pagination, Ash will return a list of records.
>
> But _with_ pagination, Ash will return an `Ash.Page.Offset` struct (for offset pagination) or `Ash.Page.Keyset` struct (for keyset pagination). Both structs contain the list of records in the `results` key of the struct.

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
- Apply [pagination](#pagination) options
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
