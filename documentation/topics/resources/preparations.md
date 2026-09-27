<!--
SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>

SPDX-License-Identifier: MIT
-->

# Preparations

Preparations are the primary way of customizing read action behavior, and are also supported by generic actions. If you are familiar with `Plug`, you can think of an `Ash.Resource.Preparation` as the equivalent of a `Plug` for queries and action inputs. At its most basic, a preparation will take a query or action input and return a new query or action input. Preparations can be simple, like adding a filter or a sort, or more complex, attaching hooks to be executed within the lifecycle of the action.

## Builtin Preparations

There are builtin preparations that can be used, and are automatically imported into your resources. See `Ash.Resource.Preparation.Builtins` for more.

The primary preparation you will use is `build/1`, which passes the arguments through to `Ash.Query.build/2` when the preparation is run. See that function for what options can be provided.

Some examples of usage of builtin preparations

```elixir
# sort by inserted at descending
prepare build(sort: [inserted_at: :desc])

# only show the top 5 results
prepare build(sort: [total_points: :desc], limit: 5)

# conditional preparation with where clause
prepare build(filter: [active: true]) do
  where argument_equals(:include_inactive, false)
end

# skip preparation if query is invalid
prepare expensive_preparation() do
  only_when_valid? true
end
```

## Custom Preparations

```elixir
defmodule MyApp.Preparations.Top5 do
  use Ash.Resource.Preparation

  # transform and validate opts
  @impl true
  def init(opts) do
    if is_atom(opts[:attribute]) do
      {:ok, opts}
    else
      {:error, "attribute must be an atom!"}
    end
  end

  @impl true
  def prepare(query, opts, _context) do
    attribute = opts[:attribute]

    query
    |> Ash.Query.sort([{attribute, :desc}])
    |> Ash.Query.limit(5)
  end
end
```

This could then be used in a resource via:

```elixir
prepare {MyApp.Preparations.Top5, attribute: :foo}
```

## Temporal Safety

Every action on a [temporal resource](/documentation/topics/advanced/temporal-resources.md) runs "as of" a point in time, which may be in the past or in the future. A preparation that runs as part of one must not assume the action is happening now. Preparations declare that they meet this bar with the `temporal_safe?/1` callback, which defaults to `false`:

```elixir
defmodule MyApp.Preparations.Top5 do
  use Ash.Resource.Preparation

  @impl true
  def temporal_safe?(_opts), do: true

  @impl true
  def prepare(query, _opts, _context) do
    # ...
  end
end
```

Running a preparation that has not declared itself temporal safe on a temporal resource raises `Ash.Error.Framework.NotTemporalSafe`. Return `true` when the preparation never reads the wall clock (use `now()` in expressions, or the query's `as_of`), has no side effects that assume the present, and performs any reads through Ash, so that `as_of` is threaded to them. `opts` is passed so a preparation that is only safe for some configurations can say so.

The builtin `build/1` and `set_context/1` preparations are temporal safe. `before_action/1`, `after_action/1` and anonymous function preparations are not, because they wrap an arbitrary function whose safety cannot be known; move that logic into a module preparation to use it on a temporal resource.

## Anonymous Function Queries

You can also use anonymous functions for preparations. This is great for prototyping, but we generally recommend using a module for organizational purposes.

```elixir
prepare fn query, _context ->
  # put your code here
end
```

## Action vs Global Preparations

You can place a preparation on a read action, like so:

```elixir
actions do
  read :read do
    prepare {Top5, attribute: :name}
  end
end
```

Or you can use the global preparations block to apply to all read actions.

```elixir
preparations do
  prepare {Top5, attribute: :name}
end
```

The preparations section allows you to add preparations across multiple actions of a resource.

## Where Clauses

Use `where` clauses to conditionally apply preparations based on validations:

```elixir
actions do
  read :search do
    argument :include_archived, :boolean, default: false
    argument :sort_by, :string, default: "name"
    
    # Only apply archived filter if not including archived items
    prepare build(filter: [archived: false]) do
      where argument_equals(:include_archived, false)
    end
    
    # Conditional sorting
    prepare build(sort: [updated_at: :desc]) do
      where argument_equals(:sort_by, "updated_at")
    end
  end
end
```

## only_when_valid? Option

Use the `only_when_valid?` option to skip preparations when the query is already invalid. This is useful for expensive preparations that should only run if validations have passed.

```elixir
actions do
  read :complex_search do
    argument :required_field, :string
    
    # This validation must pass first
    validate present(:required_field)
    
    # This expensive preparation only runs if query is valid
    prepare expensive_data_preparation() do
      only_when_valid? true
    end
  end
end
```
