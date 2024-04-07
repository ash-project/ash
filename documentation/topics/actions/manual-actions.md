# Manual Actions

Manual actions allow you to control how an action is performed instead of dispatching to a data layer. To do this, specify the `manual` option with a module that adopts the appropriate behavior.

Manual actions are a way to implement an action in a fully custom way. This can be a very useful escape hatch when you have something that you are finding difficult to model with Ash's builtin tools.

## Manual Creates/Updates/Destroy

For manual create, update and destroy actions, a module is passed that uses one of the following (`Ash.Resource.ManualCreate`, `Ash.Resource.ManualUpdate` and `Ash.Resource.ManualDestroy`).

For example:

```elixir
create :special_create do
  manual MyApp.DoCreate
end

# The implementation
defmodule MyApp.DoCreate do
  use Ash.Resource.ManualCreate

  def create(changeset, _, _) do
    record = create_the_record(changeset)
    {:ok, record}

    # An `{:error, error}` tuple should be returned if something failed
  end
end
```

The underlying record can be retrieved from `changeset.data` for update and destroy manual actions. The changeset given to the manual action will be after any `before_action` hooks, and before any `after_action` hooks.

## Manual Read Actions

Manual read actions work the same, except the will also get the "data layer query". For AshPostgres, this means you get the ecto query that would have been run. You can use `Ash.Query.apply_to/3` to apply a query to records in memory. This allows you to fetch the data in a way that is not possible with the data layer, but still honor the query that was provided to.

```elixir
# in the resource
actions do
  read :action_name do
    manual MyApp.ManualRead
    # or `{MyApp.ManualRead, ...opts}`
  end
end

# the implementation
defmodule MyApp.ManualRead do
  use Ash.Resource.ManualRead

  def read(ash_query, ecto_query, _opts, _context) do
    ...
    {:ok, query_results} | {:error, error}
  end
end
```

### Modifying the query

As an alternative to manual read actions, you can also provide the `modify_query` option, which takes an `MFA` and allows low level manipulation of the query just before it is dispatched to the data layer.

For example:

```elixir
read :read do
  modify_query {MyApp.ModifyQuery, :modify, []}
end

defmodule MyApp.ModifyQuery do
  def modify(ash_query, data_layer_query) do
    {:ok, modify_data_layer_query(data_layer_query)}
  end
end
```

This can be used as a last-resort escape hatch when you want to still use resource actions but need to do something that you can't do easily with Ash tools. As with any low level escape hatch, here be dragons.
