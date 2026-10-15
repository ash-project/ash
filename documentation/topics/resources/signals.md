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
are for "at most once" side effects like publishing to PubSub. With the default delivery,
listeners of a signal run as nested actions, during the phase the signal declares, usually inside
the emitter's transaction. If a listener fails, the emitting action fails, and its changes are
rolled back. A listener can also use another delivery, for example to run in a job. See
[Delivery](#delivery).

## Declaring signals

Signals are declared in a signal module, which is the contract between the resources that emit
them and the resources that listen to them. Both depend on it, and not on each other.

```elixir
defmodule MyApp.Shop.Signals do
  use Ash.Signals

  signals do
    signal :order_placed do
      argument :order_id, :uuid, allow_nil?: false
      argument :customer_id, :uuid, allow_nil?: false
      argument :total, :money
    end
  end
end
```

Each signal defines a struct for its arguments, `MyApp.Shop.Signals.OrderPlaced`, which is what
listeners receive. Every signal struct also has a `signal_id`, a UUIDv7 generated when the signal
is emitted, so `signal_id` can't be used as an argument name.

A signal's `phase` is when it is emitted: `:before_action`, `:after_action` (the default), or
`:after_transaction`. See the [Signals DSL documentation](/documentation/dsls/DSL-Ash.Signals.md)
for every option.

```elixir
signal :order_shipped do
  phase :after_transaction
  argument :order_id, :uuid, allow_nil?: false
end
```

## Emitting signals

Actions emit signals with `emit`, giving the signal module and the signal:

```elixir
actions do
  create :place do
    accept [:customer_id, :total]

    emit MyApp.Shop.Signals, :order_placed, fields: [order_id: :id]
  end
end
```

An action can only emit the signals it declares with `emit`, so you can always see what an action
emits by reading it.

A signal emitted in a phase other than `:after_action` must be given its phase, so that the action
shows when it emits. It must be the phase the signal declares, which is checked at compile time:

```elixir
emit MyApp.Shop.Signals, :order_shipped, phase: :after_transaction
```

Signal fields are taken from the record's attributes with the same name, so `customer_id` and
`total` above come from the order. Use these options for the others:

- `fields` - signal fields to take from other attributes of the record, like `order_id: :id`.
- `values` - signal fields to set to a value, which may be a template like `actor(:id)` or
  `arg(:reason)`. In update and destroy actions, `previous(:field)` is the value of a field before
  the action.
- `where` - validations that must pass for the signal to be emitted, like `changing(:status)`.

```elixir
update :change_status do
  accept [:status]
  require_atomic? false

  emit MyApp.Shop.Signals, :order_status_changed,
    fields: [order_id: :id],
    values: [previous_status: previous(:status), changed_by: actor(:id)],
    where: [changing(:status)]
end
```

`emit` is checked at compile time: the signal must exist, every field it sets must be a field of
the signal, every field it reads must be an attribute of the resource, and every required field of
the signal must be set. It runs in its place among the action's changes, so changes before it have
already run when it is emitted.

### Generic actions

In generic actions, signal fields are taken from the action's arguments with the same name, and
`fields` takes them from the action's result:

```elixir
action :send_invoice do
  argument :invoice_id, :uuid, allow_nil?: false

  emit MyApp.Billing.Signals, :invoice_sent

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

When building the signal needs more than `fields` and `values` can express, like a calculation,
related data, or anything else that needs loading, emit it from your own code. Declare it on the
action with `dynamic?: true`, and call `Ash.Signals.emit/5` during the signal's phase.
`Ash.Signals.emit_many/5` emits a signal for each of a list of payloads, for example from a
change's `after_batch/3` callback.

```elixir
create :place do
  accept [:customer_id, :total]

  emit MyApp.Shop.Signals, :order_placed, dynamic?: true
  change MyApp.Shop.Changes.EmitOrderPlaced
end
```

```elixir
defmodule MyApp.Shop.Changes.EmitOrderPlaced do
  use Ash.Resource.Change

  @impl true
  def change(changeset, _opts, _context) do
    Ash.Changeset.after_action(changeset, fn changeset, order ->
      order = Ash.load!(order, :customer_email)

      with :ok <-
             Ash.Signals.emit(changeset, MyApp.Shop.Signals, :order_placed, %{
               order_id: order.id,
               customer_email: order.customer_email
             }) do
        {:ok, order}
      end
    end)
  end
end
```

Emitting a signal the action doesn't declare raises an error.

Descriptions can help document dynamic signals.

```elixir
update :record_payment do
  argument :amount, :money, allow_nil?: false

  emit MyApp.Shop.Signals, :order_paid,
    dynamic?: true,
    description: "When the payment covers the rest of the order's total."

  change MyApp.Shop.Changes.RecordPayment
end
```

Listeners run as the emitting action's actor, with its tenant and `authorize?`. Pass `:actor`,
`:tenant`, `:authorize?`, `:context`, or a `:scope` to `emit/5` to change them. Include a
`signal_id` in the payload to set it yourself, for example to emit the same signal again.

## Listening to signals

Resources listen to signals in their `signals_in` section. Each listener has a name, and runs the
action with that name, unless it sets `action`:

```elixir
signals_in do
  from MyApp.Shop.Signals do
    on :order_placed, :award_points
    on :order_placed, :notify_sales, action: :notify
  end
end
```

Listener names are unique within a resource, so the same action can listen to a signal more than
once, for example with different `inputs`:

```elixir
from MyApp.Shop.Signals do
  on :order_placed, :record_buyer_entry,
    action: :record_entry,
    inputs: [account_id: :customer_id]

  on :order_placed, :record_seller_entry,
    action: :record_entry,
    inputs: [account_id: :seller_id]
end
```

What happens depends on the type of the action.

### Generic actions

A generic action receives the signal struct as its `signal` argument:

```elixir
signals_in do
  from MyApp.Shop.Signals do
    on :order_placed, :notify_warehouse
  end
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
on :order_placed, :notify_warehouse, batch?: true

action :notify_warehouse do
  argument :signals, {:array, MyApp.Shop.Signals.OrderPlaced}, allow_nil?: false
  run MyApp.Warehouse.NotifyAll
end
```

### Create actions

A create action creates a record for each signal, with its inputs taken from the signal fields of
the same name, except `signal_id`. Use `inputs` to map them, as `input: :signal_field`:

```elixir
from MyApp.Shop.Signals do
  # sets `order_id` and `customer_id` from the signal
  on :order_placed, :record_order, action: :create

  # sets `reference` from the signal's `order_id`
  on :order_placed, :open_task, inputs: [reference: :order_id]

  # records which signal created the record
  on :order_placed, :log, inputs: [:order_id, source_signal_id: :signal_id]
end
```

### Update and destroy actions

An update or destroy action runs on the records selected for each signal by a read action, the
primary read action by default. Use `args` to set the read action's arguments from the signal,
and `get_by` to select records whose fields equal fields of the signal. They can be used
together:

```elixir
from MyApp.Shop.Signals do
  on :order_placed, :mark_ordered, read_action: :active, get_by: [id: :customer_id]
  on :order_placed, :clear, read_action: :for_customer, args: [:customer_id]
end
```

If the read action selects no records for a signal, the signal is handled, and nothing happens.
Filter in the read action to only react to some signals.

### Bulk

Create, update and destroy listeners always handle the signals emitted together as bulk actions.
Update and destroy listeners run one bulk action for each distinct set of `args` and `inputs`.

### Signal path and policies

Listeners have the signals that led to them in the `signal_path` key of their context, starting
with the one they received, followed by the signals that led to it. Each entry has the signal's
`signal_module` and `signal` name, and the `resource` and `action` that emitted it. A listener
whose action emits a signal of its own adds it to the front of the path for that signal's
listeners:

```elixir
# the context of a listener of `:points_earned`, which `:earn_points` emits when it handles
# `:order_placed`
%{
  signal_path: [
    %{
      signal_module: MyApp.Loyalty.Signals,
      signal: :points_earned,
      resource: MyApp.Loyalty.PointsEntry,
      action: :earn_points
    },
    %{
      signal_module: MyApp.Shop.Signals,
      signal: :order_placed,
      resource: MyApp.Shop.Order,
      action: :place
    }
  ]
}
```

The signals themselves aren't in the path, because a listener can receive many of them at once.

Listeners run as the actor that emitted the signal, but that actor often isn't allowed to run the
listener's action directly: a customer can place an order, but shouldn't be able to award
themselves points. The `signal_in/2` check is true when the action is run by a listener of the
given signal, so the listener's policies can allow it:

```elixir
policies do
  policy action(:earn_points) do
    authorize_if signal_in(MyApp.Shop.Signals, :order_placed)
  end
end
```

`signal_in/2` only matches the signal the listener received, which is the first one in its path.
For update and destroy listeners, it is also true for the read action that selects the records
they run on.

## Delivery

A listener's `delivery` decides how signals reach it. Because it's part of the listener, each
listener decides for itself, and the emitter doesn't need to know: a listener that does slow work,
or calls an external service, can run in a job, while another listener of the same signal runs in
the emitter's transaction.

The default, `Ash.Signals.Delivery.Inline`, runs listeners immediately, as nested actions, with
each listener handling every signal before the next listener runs. During `:before_action` and
`:after_action`, they run inside the emitter's transaction: if a listener fails, the emitting
action fails and is rolled back.

Signals emitted `:after_transaction` run after the emitter's transaction has committed. If a
listener fails, the emitting action returns its error, but its changes are not rolled back. If
the emitting action is itself running inside another transaction, its `after_transaction` hooks
run while that transaction is still open, and so do the listeners.

Other deliveries are provided by extensions, and implement `Ash.Signals.Delivery`. For example, a
delivery could enqueue a job for the listener, so that it runs after the emitter's transaction
commits, and is retried when it fails:

```elixir
signals_in do
  from MyApp.Shop.Signals do
    # runs in the emitter's transaction
    on :order_placed, :record_sale

    # runs in a job, after the emitter's transaction commits
    on :order_placed, :send_receipt, delivery: {MyApp.JobDelivery, queue: :mailers}
  end
end
```

The emitter doesn't change: it emits the same signal whatever its listeners' deliveries are. If a
delivery fails, for example because it can't enqueue a job, the emit fails.

A delivery that runs listeners later must store the `signal_path` of the
`Ash.Signals.Delivery.Context` it is given, and pass it back in the `signal_path` key of the
listener's context.

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

## Mapping signals

`mix ash.signals` prints each signal, the actions that emit it, and the listeners that react to
it, with their descriptions. Pass signal modules to only print their signals, and `--format mermaid` for a Mermaid
flowchart instead:

```sh
mix ash.signals
mix ash.signals MyApp.Shop.Signals --format mermaid --output signals.mmd
```

Listeners are found wherever they are defined. Emitters are found in the resources of the domains
in your `:ash_domains` config, and in resources that listen to signals. See `Mix.Tasks.Ash.Signals`
for every option.

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

To test listeners without the action that emits the signal, `react_to_signal/4` delivers a signal
to them as if it had been emitted:

```elixir
test "placing an order awards points" do
  :ok =
    react_to_signal(
      MyApp.Shop.Signals,
      :order_placed,
      %{order_id: order.id, customer_id: customer.id},
      only: MyApp.Loyalty.Account
    )

  assert MyApp.Loyalty.points!(customer) == 100
end
```

Each listener receives the signal through its own delivery, so a listener that runs in a job only
enqueues it. Pass `inline?: true` to run every listener immediately. `only` limits the listeners to
some resources, or to `{resource, listener}` pairs, and `react_to_signal/4` takes the same
`:actor`, `:tenant` and `:authorize?` options as `Ash.Signals.emit/5`.

## Telemetry

Each emitted signal fires an `[:ash, :signal, :emitted]` telemetry event, before it is
delivered. Its metadata has the `:signal_module`, the signal's `:name`, the `:signal` struct, the
emitting `:resource` and `:action`, and the `:actor` and `:tenant` the listeners run with.
