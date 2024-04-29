# Create Actions

Create actions are used to create new records in the data layer. For example:

```elixir
# on a ticket resource
create :open do
  accept [:title]
  change set_attribute(:status, :open)
end
```

Here we have a create action called `:open` that allows setting the `title`, and sets the `status` to `:open`. It could be called like so:

```elixir
Ticket
|> Ash.Changeset.for_create(:open, %{title: "Need help!"})
|> Ash.create!()
```

See the [Code Interface guide](documentation/topics/resources/code-interfaces.md) for creating an interface to call the action more elegantly, like so:

```elixir
Support.open_ticket!("Need help!")
```

## Bulk creates

Bulk creates take a list or stream of inputs for a given action, and batches calls to the underlying data layer.

Given our example above, you could call `Ash.bulk_create` like so:

```elixir
Ash.bulk_create([%{title: "Foo"}, %{title: "Bar"}], Ticket, :open)
```

> ### Check the docs! {: .warning}
>
> Make sure to thoroughly read and understand the documentation in `Ash.bulk_create/4` before using. Read each option and note the default values. By default, bulk creates don't return records or errors, and don't emit notifications.

## Performance

Generally speaking, all regular Ash create actions are compatible (or can be made to be compatible) with bulk create actions. However, there are some important considerations.

- `Ash.Resource.Change` modules can be optimized for bulk actions by implementing `batch_change/3`, `before_batch/3` and `after_batch/3`. If you implement `batch_change/3`, the `change` function will no longer be called, and you should swap any behavior implemented with `before_action` and `after_action` hooks to logic in the `before_batch` and `after_batch` callbacks.

- Actions that reference arguments in changes, i.e `change set_attribute(:attr, ^arg(:arg))` will prevent us from using the `batch_change/3` behavior. This is usually not a problem, for instance that change is lightweight and would not benefit from being optimized with `batch_change/3`

- If your action uses `after_action` hooks, or has `after_batch/3` logic defined for any of its changes, then we _must_ ask the data layer to return the records it inserted. Again, this is not generally a problem because we throw away the results of each batch by default. If you are using `return_records?: true` then you are already requesting all of the results anyway.

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

> ### Be careful with streams {: .warning}
>
> Because streams are lazily evaluated, if you were to do something like this:
>
> ```elixir
> [input1, input2, ...] # has 300 things in it
> |> Ash.bulk_create(
>   Resource,
>   :action,
>   return_stream?: true,
>   return_records?: true,
>   batch_size: 100 #  default is 100
> )
> |> Enum.take(150) # stream has 300, but we only take 150
> ```
>
> What would happen is that we would insert 200 records. The stream would end after we process the first two batches of 100. Be sure you aren't using things like `Stream.take` or `Enum.take` to limit the amount of things pulled from the stream, unless you actually want to limit the number of records created.

## Upserts

Upserting is the process of "creating or updating" a record, modeled with a single simple create. Both bulk creates and regular creates support upserts. Upserts can be declared in the action, like so:

```elixir
create :create_user do
  accept [:email]
  upsert? true
  upsert_identity :unique_email
end
```

Or they can be done with options when calling the create action.

```elixir
Ash.create!(changeset, upsert?: true, upsert_identity: :unique_email)
```

> ### Upserts do not use an update action {: .warning}
>
> While an upsert is conceptually a "create or update" operation, it does not result in an update action being called. The data layer contains the upsert implementation. This means that if you have things like global changes that are only run on update, they will not be run on upserts that result in an update. Additionally, notifications for updates will not be emitted from upserts.

### Atomic Updates

Upserts support atomic updates. These atomic updates _do not apply to the data being created_. They are only applied in the case of an update. For example:

```elixir
create :create_game do
  accept [:identifier]
  upsert? true
  upsert_identity :identifier
  change set_attribute(:score, 0)
  change atomic_update(:score, expr(score + 1))
end
```

This will result in creating a game with a score of 0, and if the game already exists, it will increment the score by 1.

For information on options configured in the action, see `d:Ash.Resource.Dsl.actions.create`.
For information on options when calling the action, see `Ash.create/2`.

## What happens when you run a create Action

All actions are run in a transaction if the data layer supports it. You can opt out of this behavior by supplying `transaction?: false` when creating the action. When an action is being run in a transaction, all steps inside of it are serialized because transactions cannot be split across processes.

- Authorization is performed on the changes
- A before action hook is added to set up belongs_to relationships that are managed. This means potentially creating/modifying the destination of the relationship, and then changing the `destination_attribute` of the relationship.
- `before_transaction` and `around_transaction` hooks are called (`Ash.Changeset.before_transaction/2`). Keep in mind, any validations that are marked as `before_action? true` (or all global validations if your action has `delay_global_validations? true`) will not have happened at this point.
- A transaction is opened if the action is configured for it (by default they are) and the data layer supports transactions
- `before_action` hooks are performed in order
- The main action is sent to the data layer
- `after_action` hooks are performed in order
- Non-belongs-to relationships are managed, creating/updating/destroying related records.
- The transaction is closed, if one was opened
- `after_transaction` hooks are invoked with the result of the transaction (even if it was an error)
