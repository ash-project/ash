# Preparations

Preparations are the primary way of customizing read action behavior. If you are familiar with `Plug`, you can think of an `Ash.Resource.Preparation` as the equivalent of a `Plug` for queries. At its most basic, a preparation will take a query and return a new query. Queries can be simple, like adding a filter or a sort, or more complex, attaching hooks to be executed within the lifecycle of the action.

## Builtin Preparations

There are builtin preparations that can be used, and are automatically imported into your resources. See `Ash.Resource.Preparation.Builtins` for more.

The primary preparation you will use is `build/1`, which passes the arguments through to `Ash.Query.build/2` when the preparation is run. See that function for what options can be provided.

Some examples of usage of builtin preparations

```elixir
# sort by inserted at descending
prepare build(sort: [inserted_at: :desc])

# only show the top 5 results
prepare build(sort: [total_points: :desc], limit: 5)
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
