<!--
SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>

SPDX-License-Identifier: MIT
-->

# Signals

Signals let one resource react to what happens in another, without either depending on the
other. A resource emits a typed signal, like `:order_placed`, and any resource can listen to it
with one of its own actions.

Signals are useful when one part of your application needs to react to another part, but the
part that acts shouldn't have to know about the reactions. For example, when an order is placed,
a loyalty program might award points, a ticketing system might open a task, and a reporting
system might record the sale. With signals, the order resource emits `:order_placed`, and each of
those resources listens to it. The order resource doesn't change when a new reaction is added.

## Signals vs. calling another resource

If a resource needs something from another resource to finish its action, call that resource
directly, for example with its code interface. Signals are fire and forget: the emitter gets
nothing back from its listeners, so it doesn't depend on what they do, or on whether anything
listens at all. Needing a value back from a listener is a sign that a signal is the wrong tool.

## Signals vs. notifiers

[Notifiers](/documentation/topics/resources/notifiers.md) run after the transaction commits, and
are for "at most once" side effects like publishing to PubSub. Listeners of a signal run as
nested actions, during the phase the signal declares, usually inside the emitter's transaction.
If a listener fails, the emitting action fails, and its changes are rolled back.

## Declaring signals

Signals are declared in a signal module, which is the contract between the resources that emit
them and the resources that listen to them. Both depend on it, and not on each other.

```elixir
defmodule MyApp.Shop.Signals do
  use Ash.Signals

  signals do
    signal :order_placed do
      phase :after_action
      argument :order_id, :uuid, allow_nil?: false
      argument :customer_id, :uuid, allow_nil?: false
      argument :total, :money
    end
  end
end
```

Each signal defines a struct for its arguments, `MyApp.Shop.Signals.OrderPlaced`, which is what
listeners receive.

A signal's `phase` is when it is emitted: `:before_action`, `:after_action`, or
`:after_transaction`. It tells listeners whether they run inside the emitter's transaction, before
or after its data layer action. See the [Signals DSL documentation](/documentation/dsls/DSL-Ash.Signals.md)
for every option.

## Emitting signals

Emit signals with the `emit_signal` change, giving the signal module, the signal, and its phase:

```elixir
actions do
  create :place do
    accept [:customer_id, :total]

    change emit_signal(MyApp.Shop.Signals, :order_placed, :after_action,
             fields: [order_id: :id]
           )
  end
end
```

The phase must be the one the signal declares. It's repeated so that the action shows when it
emits, and it is checked at compile time.

Signal fields are taken from the record's fields with the same name, so `customer_id` and `total`
above come from the order. Use these options for the others:

- `fields` - signal fields to take from other fields of the record, like `order_id: :id`.
- `values` - signal fields to set to a value, which may be a template like `actor(:id)` or
  `arg(:reason)`. In update and destroy actions, `previous(:field)` is the value of a field before
  the action.

```elixir
update :change_status do
  accept [:status]
  require_atomic? false

  change emit_signal(MyApp.Shop.Signals, :order_status_changed, :after_action,
           fields: [order_id: :id],
           values: [previous_status: previous(:status), changed_by: actor(:id)]
         ),
         where: [changing(:status)]
end
```

Use `where` to emit only in some cases, like any other change.

`emit_signal` is checked at compile time: the signal must exist, every field it sets must be a
field of the signal, every field it reads must be a field of the resource, and every required
field of the signal must be set. Using it also adds the signal module to the resource's
`signals_out`.

### Generic actions

Generic actions emit signals with the `emit_signal` preparation. Signal fields are taken from the
action's arguments with the same name, and `fields` takes them from the action's result:

```elixir
action :send_invoice do
  argument :invoice_id, :uuid, allow_nil?: false

  prepare emit_signal(MyApp.Billing.Signals, :invoice_sent, :after_action)

  run MyApp.Billing.SendInvoice
end
```

### Bulk actions

Bulk actions emit their signals together, once per batch, so listeners can handle them in bulk
too. Signals emitted `:after_transaction` are the exception, and are emitted once per record.

### Atomic actions

Signals emitted `:after_action` work with atomic actions. Signals emitted `:before_action` or
`:after_transaction`, and `previous/1`, need things atomic actions don't have, so an action using
them can't be atomic. Set `require_atomic? false` on update and destroy actions that use them.

### Emitting from your own code

To emit from your own changes, list the signal module in the resource's `signals_out`, and call
`Ash.Signals.emit/5` during the signal's phase. `Ash.Signals.emit_many/5` emits a signal for each
of a list of payloads, for example from a change's `after_batch/3` callback.

```elixir
use Ash.Resource, signals_out: [MyApp.Shop.Signals]

# in a change
Ash.Changeset.after_action(changeset, fn changeset, order ->
  with :ok <-
         Ash.Signals.emit(changeset, MyApp.Shop.Signals, :order_placed, %{
           order_id: order.id,
           customer_id: order.customer_id
         }) do
    {:ok, order}
  end
end)
```

Listeners run as the emitting action's actor, with its tenant and `authorize?`. Pass `:actor`,
`:tenant`, `:authorize?`, `:context`, or a `:scope` to `emit/5` to change them.

## Listening to signals

Resources listen to signals in their `signals_in` section, with one of their own actions:

```elixir
signals_in do
  on MyApp.Shop.Signals, :order_placed, :award_points
end
```

What happens depends on the type of the action.

### Generic actions

A generic action receives the signal struct as its `signal` argument:

```elixir
signals_in do
  on MyApp.Shop.Signals, :order_placed, :notify_warehouse
end

actions do
  action :notify_warehouse do
    argument :signal, MyApp.Shop.Signals.OrderPlaced, allow_nil?: false
    run MyApp.Warehouse.Notify
  end
end
```

With `batch?: true`, it is called once with every signal emitted together, as its `signals`
argument:

```elixir
on MyApp.Shop.Signals, :order_placed, :notify_warehouse, batch?: true

action :notify_warehouse do
  argument :signals, {:array, MyApp.Shop.Signals.OrderPlaced}, allow_nil?: false
  run MyApp.Warehouse.NotifyAll
end
```

### Create actions

A create action creates a record for each signal, with its inputs taken from the signal fields of
the same name. Use `inputs` to map them, as `input: :signal_field`:

```elixir
signals_in do
  # sets `order_id` and `customer_id` from the signal
  on MyApp.Shop.Signals, :order_placed, :create

  # sets `reference` from the signal's `order_id`
  on MyApp.Shop.Signals, :order_placed, :open_task, inputs: [reference: :order_id]
end
```

### Update and destroy actions

An update or destroy action runs on the records selected for each signal by a read action, the
primary read action by default. Use `args` to set the read action's arguments from the signal,
and `get_by` to select records whose fields equal fields of the signal. They can be used
together:

```elixir
signals_in do
  on MyApp.Shop.Signals, :order_placed, :mark_ordered,
    read_action: :active,
    get_by: [id: :customer_id]

  on MyApp.Shop.Signals, :order_placed, :clear, read_action: :for_customer, args: [:customer_id]
end
```

If the read action selects no records for a signal, the signal is handled, and nothing happens.
Filter in the read action to only react to some signals.

### Bulk

Create, update and destroy listeners always handle the signals emitted together as bulk actions.
Update and destroy listeners run one bulk action for each distinct set of `args` and `inputs`.

## Transactions and errors

Listeners run immediately, as nested actions, with each listener handling every signal before
the next listener runs. During `:before_action` and `:after_action`, they run inside the
emitter's transaction: if a listener fails, the emitting action fails and is rolled back.

Signals emitted `:after_transaction` run after the emitter's transaction has committed. If a
listener fails, the emitting action returns its error, but its changes are not rolled back. If
the emitting action is itself running inside another transaction, its `after_transaction` hooks
run while that transaction is still open, and so do the listeners.

## Setup

Add the `:ash_signals` compiler to your project:

```elixir
def project do
  [
    compilers: [:ash_signals] ++ Mix.compilers(),
    ...
  ]
end
```

Like protocol consolidation, it finds the listeners of each signal once per build, and compiles
them into a module, so that emitting a signal doesn't have to search for them. Without it,
listeners are searched for every time a signal is emitted, which is much slower.

If you use the Phoenix code reloader, add it to your endpoint's reloadable compilers too, so that
listeners added during development are picked up:

```elixir
config :my_app, MyAppWeb.Endpoint,
  reloadable_compilers: [:ash_signals, :phoenix_live_view, :gettext, :elixir, :app]
```

Listeners defined after compilation, like resources defined inside tests, aren't part of the
consolidated modules. To use them, disable consolidation in that environment:

```elixir
def project do
  [
    ash: [consolidate_signals: Mix.env() != :test],
    ...
  ]
end
```

## Testing

`Ash.Signals.Test` has helpers for asserting on emitted signals. `assert_emits_signal/4` and
`refute_emits_signal/4` check the signals emitted by a function:

```elixir
import Ash.Signals.Test

test "placing an order emits order_placed" do
  order =
    assert_emits_signal MyApp.Shop.Signals, :order_placed, %{order_id: order_id}, fn ->
      MyApp.Shop.place_order!(customer, 100)
    end

  assert order.id == order_id
end
```

`assert_signal_emitted/4` and `refute_signal_emitted/4` check the signals emitted so far, after
calling `capture_signals/1`, for example with `setup :capture_signals`.

## Telemetry

Each emitted signal fires an `[:ash, :signal, :emitted]` telemetry event, before its listeners
run. Its metadata has the `:signal_module`, the signal's `:name`, the `:signal` struct, the
emitting `:resource` and `:action`, and the `:actor` and `:tenant` the listeners run with.
