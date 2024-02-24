# Bulk Actions

Bulk actions are ways to create, update or destroy many records at once, backed by scalable patterns.

Currently, only bulk creates are implemented. Bulk updates and bulk destroys will come next.

## Bulk Creates

Bulk creates take a list or stream of inputs for a given action, and batches calls to the underlying data layer. For example, with an action like this:

```elixir
create :create do
  accept [:title, :subtitle]
end
```

You could then call `Ash.bulk_create` like so:

```elixir
Ash.bulk_create([ %{title: "foo", subtitle: "bar"}, %{title: "baz", subtitle: "buz"}], Resource, :action)
```

## Considerations

Generally speaking, all regular Ash create actions are compatible (or can be made to be compatible) with bulk create actions. However, there are some important considerations.

- `Ash.Resource.Change` modules can be optimized for bulk actions by implementing `batch_change/3`, `before_batch/3` and `after_batch/3`. If you implement `batch_change/3`, the `change` function will no longer be called, and you should swap any behavior implemented with `before_action` and `after_action` hooks to logic in the `before_batch` and `after_batch` callbacks.

- Actions that reference arguments in changes, i.e `change set_attribute(:attr, ^arg(:arg))` will prevent us from using the `batch_change/3` behavior. This is usually not a problem, for instance that change is lightweight and would not benefit from being optimized with `batch_change/3`

- If your action uses `after_action` hooks, or has `after_batch/3` logic defined for any of its changes, then we *must* ask the data layer to return the records it inserted. Again, this is not generally a problem because we throw away the results of each batch by default. If you are using `return_records?: true` then you are already requesting all of the results anyway.

## Returning a Stream

Returning a stream allows you to work with a bulk action as an Elixir Stream. For example:

```elixir
input_stream()
|> Ash.bulk_create(Resource, :action, return_stream?: true, return_records?: true)
|> Stream.map(fn {:ok, result} -> 
  # process results
  {:error, error} ->
  # process errors
end)
|> Enum.reduce(%{}, fn {:ok, result}, acc -> 
   # process results
   {:error, error} ->
   # process errors
end)
```

### Considerations

Because streams are lazily evaluated, if you were to do something like this:

```elixir
[input1, input2, ...] # has 300 things in it
|> Ash.bulk_create(Resource, :action, return_stream?: true, return_records?: true, batch_size: 100) # the default is 100
|> Enum.take(150)
```

What would happen is that we would insert 200 records (assuming no errors were emitted). Because the stream would end after we process the first two batches. If you want to make sure that everything happens, just be sure you aren't using things like `Stream.take` or `Enum.take` to limit the amount of things pulled from the stream.