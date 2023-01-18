# Pagination

Pagination is configured at the action level. There are two kinds of pagination supported: `keyset` and `offset`. There are
pros and cons to each. An action can support both at the same time, or only one (or none). A full count of records can be
requested by passing `page: [count: true]`, but it should be kept in mind that doing this requires running the same query
twice, one of which is a count of all records. Ash does these in parallel, but it can still be quite expensive on large
datasets. For more information on the options for configuring actions to support pagination, see `d:Ash.Resource.actions.read|prepare`

## Offset Pagination

Offset pagination is done via providing a `limit` and an `offset`. A `limit` is how many records that should be returned on the page.
An `offset` is how many records from the beginning should be skipped. Using this, you might make requests like the following:

```elixir
# Get the first ten records
Api.read(Resource, page: [limit: 10])
# Get the second ten records
Api.read(Resource, page: [limit: 10, offset: 10])
# No need to do this in practice, see `c:Ash.Api.page/2`
```

### Offset Pros

- Simple to think about
- Possible to skip to a page by number. E.g the 5th page of 10 records is `offset: 40`
- Easy to reason about what page you are currently on (if the total number of records is requested)
- Can go to the last page (even though, if done by using the full count, the data could have changed)

### Offset Cons

- Does not perform well on large datasets (if you have to ask if your dataset is "large", it probably isn't)
- When moving between pages, if data was created or deleted, records may appear on multiple pages

## Keyset Pagination

Keyset pagination is done via providing an `after` or `before` option, as well as a `limit`. The value of this option should be
a `keyset` that has been returned from a previous request. Keysets are returned when a request is made with a `limit` to an action
that supports `keyset` pagination, and they are stored in the `__metadata__` key of each record. The `keyset` is a special value that
can be passed into the `after` or `before` options, to get records that occur after or before.

For example:

```elixir
page = Api.read(Resource, page: [limit: 10])

last_record = List.last(page.results)

# No need to do this in practice, see `c:Ash.Api.page/2`
next_page = Api.read(Resource, page: [limit: 10, after: last_record.__metadata__.keyset])
```

### Keyset Pros

- Performs very well on large datasets (assuming indices exist on the columns being sorted on)
- Behaves well as data changes. The record specified will always be the first or last item in the page

### Keyset Cons

- A bit more complex to use
- Can't go to a specific page number
