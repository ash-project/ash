# Update Actions

Update actions are used to update records in the data layer. For example:

```elixir
# on a ticket resource
update :close do
  accept [:close_reason]
  change set_attribute(:status, :closed)
end
```

Here we have an update action called `:close` that allows setting the `close_reason`, and sets the `status` to `:closed`. It could be called like so:

```elixir
ticket # providing an initial ticket to close
|> Ash.Changeset.for_update(:close, %{close_reason: "I figured it out."})
|> Ash.update!()
```

See the [Code Interface guide](documentation/topics/resources/code-interfaces.md) for creating an interface to call the action more elegantly, like so:

```elixir
Support.close_ticket!(ticket, "I figured it out.")
# You can also provide an id
Support.close_ticket!(ticket.id, "I figured it out.")
```

## Atomics

Atomic updates can be added to a changeset, which will update the value of an attribute given by an expression. Atomics can be a very powerful way to model updating data in a simple way. An action does not have to be [fully atomic](#fully-atomic-updates) in order to leverage atomic updates. For example:

```elixir
update :add_to_name do
  argument :to_add, :string, allow_nil? false
  change atomic_update(:name, expr("#{name}_#{to_add}"))
end
```

Changing attributes in this way makes them safer to use in concurrent environments, and is typically more performant than doing it manually in memory.

> ### Atomics are not stored with other changes {: .warning}
>
> While we recommend using atomics wherever possible, it is important to note that they are stored in their own map in the changeset, i.e `changeset.atomics`, meaning if you need to do something later in the action with the new value for an attribute, you won't be able to access the new value. This is because atomics are evaluated in the data layer.

## Fully Atomic updates

Atomic updates are a special case of update actions that can be done atomically. If your update action can't be done atomically, you will get an error unless you have set `require_atomic? false`. This is to encourage you to opt for atomic updates whereever reasonable. Not all actions can reasonably be made atomic, and not all non-atomic actions are problematic for concurrency. The goal is only to make sure that you are aware and have considered the implications.

> ### What does atomic mean? {: .info}
>
> An atomic update is one that can be done in a single operation in the data layer. This ensures that there are no issues with concurrent access to the record being updated, and that it is as performant as possible.
> For example, the following action cannot be done atomically, because it has
> an anonymous function change on it.
>
> ```elixir
> update :increment_score do
>   change fn changeset, _ ->
>     Ash.Changeset.set_attribute(changeset, :score, changeset.data.score + 1)
>   end
> end
> ```
>
> The action shown above is not safe to run concurrently. If two separate processes fetch the record with score `1`, and then call `increment_score`, they will both set the score to `2`, when what you almost certainly intended to do was end up with a score of `3`
>
> By contrast, the following action _can_ be done atomically
>
> ```elixir
> update :increment_score do
>   change atomic_update(:score, expr(score + 1)
> end
> ```
>
> In a SQL data layer, this would produce SQL along the lines of
>
> ```elixir
> "UPDATE table SET score = score + 1 WHERE id = post_id"
> ```

## What makes an action not atomic?

### Types that can't be atomically casted

Not all types support being casted atomically. For instance, `:union` types, and embedded resources that have primary keys(and therefore may need to use an update action) cannot currently be casted atomically.

### Changes without an `atomic` callback

Changes can be enhanced to support atomics by defining `c:Ash.Resource.Change.atomic/3`. This callback can return a map of atomic updates to be made to attributes. Here is a simplified example from the built in `Ash.Resource.Change.Builtins.increment/2` change:

```elixir
@impl true
def atomic(_changeset, opts, _context) do
  # Set the requested attribute to its current value (atomic_ref) + the amount
  {:atomic, %{opts[:attribute] => expr(^atomic_ref(opts[:attribute]) + ^opts[:amount])}}
end
```

### Validations without an `atomic` callback

Validations can be enhanced to support atomics by defining `c:Ash.Resource.Validation.atomic/3`. This callback can return an atomic validation (or a list of atomic validations), which is represented by a list of affected attributes (not currently used), an expression that should trigger an error, and the expression producing the error. Here is an example from the built in `Ash.Resource.Validations.Builtins.attribute_equals/2` validation:

```elixir
@impl true
def atomic(_changeset, opts, context) do
  {:atomic, [opts[:attribute]], expr(^atomic_ref(opts[:attribute]) != ^opts[:value]),
   expr(
     error(^InvalidAttribute, %{
       field: ^opts[:attribute],
       value: ^atomic_ref(opts[:attribute]),
       message: ^(context.message || "must equal %{value}"),
       vars: %{field: ^opts[:attribute], value: ^opts[:value]}
     })
   )}
end
```

## Bulk updates

There are three strategies for bulk updating data. They are, in order of preference: `:atomic`, `:atomic_batches`, and `:stream`. When calling `Ash.bulk_update/4`, you can provide a strategy or strategies that can be used, and Ash will choose the best one available. The implementation of the update action and the capabilities of the data layer determine what strategies can be used.

## Atomic

Atomic bulk updates are used when the subject of the bulk update is a query, and the update action [can be done atomically](#fully-atomic-updates) and the data layer supports updating a query. They map to a single statement to the data layer to update all matching records. The data layer must support updating a query.

### Example

```elixir
Ticket
|> Ash.Query.filter(status == :open)
|> Ash.bulk_update!(:close, %{reason: "Closing all open tickets."})
```

If using a SQL data layer, this would produce a query along the lines of

```sql
UPDATE tickets
SET status = 'closed',
    reason = 'Closing all open tickets.'
WHERE status = 'open';
```

## Atomic Batches

Atomic batches is used when the subject of the bulk update is an enumerable (i.e list or stream) of records and the update action [can be done atomically](#fully-atomic-updates) and the data layer supports updating a query. The records are pulled out in batches, and then each batch follows the logic described [above](#atomic). The batch size is controllable by the `batch_size` option.

### Example

```elixir

Ash.bulk_update!(one_hundred_tickets, :close, %{reason: "Closing all open tickets."}, batch_size: 10)
```

If using a SQL data layer, this would produce ten queries along the lines of

```sql
UPDATE tickets
SET status = 'closed',
    reason = 'Closing all open tickets.'
WHERE id IN (...ids)
```

## Stream

Stream is used when the update action [cannot be done atomically](#fully-atomic-updates) or if the data layer does not support updating a query. If a query is given, it is run and the records are used as an enumerable of inputs. If an enumerable of inputs is given, each one is updated individually. There is nothing inherently wrong with doing this kind of update, but it will naturally be slower than the other two strategies.
The benefit of having a single interface (`Ash.bulk_update/4`) is that the caller doesn't need to change based on the performance implications of the action.

## Running a standard update action

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

## Atomic Upgrade

Update actions that are run as "normal" update actions will, at the time of execution, be "upgraded" to an atomic action if possible. This  means taking the original inputs and building a corresponding atomic action. This behavior is primarily useful for using things like `AshPhoenix.Form`, where you want to validate and see the effects of an action before running it, but want the ultimate invocation to be atomic (i.e concurrency safe).

You can disable this by adding `atomic_upgrade? false` to the action configuration. Additionally, you may want to configure the read action used for atomic upgrades (defaults to the primary read), with `atomic_upgrade_with` option, i.e `atomic_upgrade_with :list_all`
