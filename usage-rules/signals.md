<!--
SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>

SPDX-License-Identifier: MIT
-->

# Signals

Signals let one resource react to what happens in another without either depending on the other. A signal module declares typed signals, resources emit them from their actions, and other resources listen to them with their own actions. 

## When to Use Signals

- Use a signal when other resources should react to an action, and the resource taking the action shouldn't know about them. For example, awarding loyalty points when an order is placed. Prefer this to calling those resources from an `after_action` hook.
- Don't use a signal when the action needs something back from the other resource to finish. Call that resource directly, for example through its code interface. Signals are fire and forget: the emitter gets nothing back from its listeners.
- Use a notifier, not a signal, for "at most once" side effects after the transaction commits, like publishing to PubSub.
- With the default delivery, listeners of `:before_action` and `:after_action` signals run as nested actions inside the emitter's transaction. A failing listener fails the emitting action and rolls it back.

## Declaring Signals

```elixir
defmodule MyApp.Shop.Signals do
  use Ash.Signals

  signals do
    signal :order_placed do
      argument :order_id, :uuid, allow_nil?: false
      argument :customer_id, :uuid, allow_nil?: false
    end
  end
end
```

- Declare signals in a signal module, not in a resource. Emitters and listeners both depend on the signal module, and never on each other.
- Each signal defines a struct, like `MyApp.Shop.Signals.OrderPlaced`, which is what listeners receive.
- Every signal has a `signal_id` field, a UUIDv7 generated when it is emitted. Don't declare an argument named `signal_id`.
- A signal's `phase` is when it is emitted: `:before_action`, `:after_action` or `:after_transaction`. It defaults to `:after_action`, which you should prefer. `:before_action` and `:after_transaction` signals make update and destroy actions non-atomic, and in bulk actions, `:after_transaction` signals are emitted once per record instead of once per batch.

## Emitting Signals

```elixir
create :place do
  accept [:customer_id, :total]

  emit MyApp.Shop.Signals, :order_placed, fields: [order_id: :id]
end
```

- Emit signals with `emit` in create, update, destroy and generic actions. An action can only emit the signals it declares with `emit`.
- For a signal emitted in another phase than `:after_action`, pass the phase, as in `emit MyApp.Shop.Signals, :order_shipped, phase: :after_transaction`. It must match the phase the signal declares.
- Signal fields are taken from the record's attributes of the same name. In generic actions, they are taken from the action's arguments, and `fields` takes them from the action's result.
- Use `fields: [signal_field: :attribute]` to take signal fields from other attributes, and `values: [signal_field: value]` to set them to values or templates like `actor(:id)`, `arg(:reason)` or `previous(:status)`.
- Use `where` to emit only in some cases: `emit MyApp.Shop.Signals, :order_status_changed, where: [changing(:status)]`.
- `emit` runs in its place among the action's changes, and is checked at compile time.
- An update or destroy that emits a `:before_action` or `:after_transaction` signal, or uses `previous/1`, can't be atomic and needs `require_atomic? false`. `:after_action` signals don't need it.
- `fields` only reads attributes. When the signal needs a calculation, an aggregate, related data or any other transformation, emit it from your own code: declare it with `emit MyApp.Shop.Signals, :order_placed, dynamic?: true`, and call `Ash.Signals.emit/5` from a change during the signal's phase, or `Ash.Signals.emit_many/5` from `after_batch/3`. Emitting a signal the action doesn't declare raises an error.
- Give dynamic emits a `description` saying when they're emitted, since the action can't show it: `emit MyApp.Shop.Signals, :order_paid, dynamic?: true, description: "When the payment covers the rest of the order's total."`

## Listening to Signals

```elixir
signals_in do
  from MyApp.Shop.Signals do
    on :order_placed, :award_points
    on :order_placed, :open_task, action: :create, inputs: [reference: :order_id]
    on :order_placed, :mark_ordered, read_action: :active, get_by: [id: :customer_id]
  end
end
```

- Each `on` has a name, unique within the resource, and runs the action with that name unless `action` is set.
- A **generic action** receives the signal struct as its `signal` argument. With `batch?: true`, it is called once with every signal emitted together, as its `signals` argument.

  ```elixir
  action :award_points do
    argument :signal, MyApp.Shop.Signals.OrderPlaced, allow_nil?: false
    run MyApp.Loyalty.AwardPoints
  end
  ```

- A **create action** creates a record for each signal. Its inputs are taken from the signal fields of the same name, except `signal_id`. Map others with `inputs: [input: :signal_field]`.
- An **update or destroy action** runs on the records selected for each signal by a read action, the primary read action unless `read_action` is set. Select them with `get_by: [field: :signal_field]`, `args: [read_argument: :signal_field]`, or both. If no records are selected, nothing happens. To react to only some signals, filter in the read action.
- Create, update and destroy listeners handle the signals emitted together as bulk actions.
- A listener's `delivery` decides how signals reach it. The default, `Ash.Signals.Delivery.Inline`, runs it immediately, during the signal's phase. Other deliveries come from extensions and implement `Ash.Signals.Delivery`, for example to run the listener in a job: `on :order_placed, :send_receipt, delivery: {SomeJobDelivery, queue: :mailers}`. Each listener chooses its own delivery, and the emitter doesn't change.
- Run a listener in a job when it does slow work or calls external services. Keep it inline when its changes must be committed with the emitter's, or when its failure should fail the emitting action.
- Listeners run with the emitting action's actor, tenant and `authorize?`. The listener resource's policies are checked against that actor, so write them with the emitting actor in mind.
- A listener's return value is ignored. Don't design listeners to return something the emitter needs.

## Setup

- Add the `:ash_signals` compiler to the project: `compilers: [:ash_signals] ++ Mix.compilers()`. Without it, listeners are searched for every time a signal is emitted, which is much slower.
- With the Phoenix code reloader, add `:ash_signals` to the endpoint's `reloadable_compilers` too.
- Listeners defined after compilation, like resources defined inside tests, aren't found when signals are consolidated. If tests define listeners, set `ash: [consolidate_signals: Mix.env() != :test]` in the project config.
- To see what emits and listens to each signal, run `mix ash.signals`, optionally with signal modules to limit it to, or `--format mermaid` for a flowchart.

## Testing

- Use `Ash.Signals.Test`. `assert_emits_signal/4` and `refute_emits_signal/4` run a function, check the signals it emitted, and return its result.
- `assert_signal_emitted/4` and `refute_signal_emitted/4` check the signals emitted so far, after `setup :capture_signals`.
- Test a listener without the action that emits its signal with `react_to_signal/4`, which delivers a signal to the listeners as if it had been emitted: `react_to_signal(MyApp.Shop.Signals, :order_placed, %{order_id: id, customer_id: customer_id}, only: MyApp.Loyalty.Account)`. Listeners receive it through their own delivery, so one that runs in a job only enqueues it; pass `inline?: true` to run them all immediately.

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
