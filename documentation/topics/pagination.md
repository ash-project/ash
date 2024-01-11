# Pagination

Pagination when reading records is configured on a per-action basis. Ash supports two kinds of pagination: `keyset` and `offset`.

A single action can use both kinds of pagination if desired, but typically you would use one or the other.

For information on configuring actions to support pagination, see `d:Ash.Resource.Dsl.actions.read|prepare`.

> #### Counting records {: .tip}
>
> When calling an action that uses pagination, the full count of records can be requested by adding the option `page: [count: true]`.
> Note that this will perform a similar query a second time to fetch the count, which can be expensive on large data sets.

## Offset Pagination

Offset pagination is done via providing a `limit` and an `offset` when making queries.

* The `limit` determines how many records should be returned in the query.
* The `offset` describes how many records from the beginning should be skipped.

Using this, you might make requests like the following:

```elixir
# Get the first ten records
Api.read(Resource, page: [limit: 10])
# or by using an action named `read` directly
Resource.read(page: [limit: 10])

# Get the next ten records
Api.read(Resource, page: [limit: 10, offset: 10])
# or by using an action named `read` directly
Resource.read(page: [limit: 10, offset: 10])
```

Next/previous page requests can also be made in memory, using an existing page of search results:

```elixir
# Return page three of search results
{:ok, third_page} = Resource.read(page: [limit: 10, offset: 20])

# Use `:prev` and `:next` to go backwards and forwards.
# `:first`, `:last` and specifying a page number are also supported.
{:ok, second_page} = Api.page(third_page, :prev)
{:ok, fourth_page} = Api.page(third_page, :next)
```

### Pros of offset pagination

- Simple to think about
- Possible to skip to a page by number. E.g the 5th page of 10 records is `offset: 40`
- Easy to reason about what page you are currently on (if the total number of records is requested)
- Can go to the last page (though data may have changed between calculating the last page details, and requesting it)

### Cons of offset pagination

- Does not perform well on large datasets (if you have to ask if your dataset is "large", it probably isn't)
- When moving between pages, if data was created or deleted, individual records may be missing or appear on multiple pages

## Keyset Pagination

Keyset pagination is done via providing an `after` or `before` option, as well as a `limit`.

* The `limit` determines how many records should be returned in the query.
* The `after` or `before` value should be a `keyset` value that has been returned from a previous request. Keyset values are returned when a request is made with a `limit` to an action that supports keyset pagination, and they are stored in the `__metadata__` key of each record.

For example:

```elixir
{:ok, page} = Api.read(Resource, page: [limit: 10])
# Returns `{:ok, %Ash.Page.Keyset{results: [...], before: nil, after: nil}}`
# The `before`/`after` values are the keysets used for this request.

# Fetch the keyset for the next request from the results list
last_record = List.last(page.results)
# Returns `%Resource{__metadata__: %{keyset: "g2wAAAABbQAAACQzOWNjNTcwNy00NjlmL..."}, ...}``

# Use this keyset value to fetch the next page
{:ok, next_page} = Api.read(Resource, page: [limit: 10, after: last_record.__metadata__.keyset])
```

Like offset pagination, next/previous page requests can also be made in memory, using an existing page of search results:

```elixir
# Return page three of search results
{:ok, third_page} = Resource.read(page: [limit: 10])

# Use `:prev` and `:next` to go backwards and forwards.
# `:first` can also be used, but `:last` and specifying a page number are not supported.
{:ok, second_page} = Api.page(third_page, :prev)
{:ok, fourth_page} = Api.page(third_page, :next)
```

### Pros of keyset pagination

- Performs very well on large datasets (assuming indices exist on the columns being sorted on)
- Behaves well as data changes. The record specified will always be the first or last item in the page

### Cons of keyset paginations

- A bit more complex to use
- Can't go to a specific page number

## Example implementation

### Setting up the resource

Add the `pagination` macro call to the [action](glossary.md#action) of the [resource](glossary.md#resource) that you want to be paginated.

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

### Querying the paginated resource

For all available querying options, see [`Ash.Api.read.pagination`](https://hexdocs.pm/ash/Ash.Api.html#c:read/2-pagination).

> #### Check the updated query return type! {: .info}
> Pagination will modify the return type of calling the query action.
>
> Without pagination, Ash will return a list of records.
>
> But _with_ pagination, Ash will return an `Ash.Page.Offset` struct (for offset pagination) or `Ash.Page.Keyset` struct (for keyset pagination). Both structs contain the list of records in the `results` key of the struct.
