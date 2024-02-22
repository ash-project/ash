# Reactor

`Ash.Reactor` is an extension for [`Reactor`](https://github.com/ash-project/reactor) which adds explicit support for interacting with resources via their defined actions.

See [Getting started with Reactor](https://hexdocs.pm/reactor/getting-started-with-reactor.html) for more information about Reactor.

## Usage

You can either add the `Ash.Reactor` extension to your existing reactors eg:

```elixir
defmodule MyExistingReactor do
  use Reactor, extensions: [Ash.Reactor]
end
```

or for your convenience you can use `use Ash.Reactor` which expands to exactly the same as above.

## Examples

An example is worth 1000 words of prose:

```elixir
defmodule ExampleReactor do
  use Ash.Reactor

  ash do
    default_api ExampleApi
  end

  input :customer_name
  input :customer_email
  input :plan_name
  input :payment_nonce

  create :create_customer, Customer do
    inputs %{name: input(:customer_name), email: input(:customer_email)}
  end

  get :get_plan, Plan, :get_plan_by_name do
    inputs %{name: input(:plan_name)}
    fail_on_not_found? true
  end

  action :take_payment, PaymentProvider do
    inputs %{
      nonce: input(:payment_nonce),
      amount: result(:get_plan, [:price])
    }
  end

  create :subscription, Subscription do
    inputs %{
      plan_id: result(:get_plan, [:id]),
      payment_provider_id: result(:take_payment, :id)
    }
  end
end
```

## Actions

For each action type there is a corresponding step DSL, which needs a name (used
to refer to the result of the step by other steps), a resource and optional
action name (defaults to the primary action if one is not provided).

Actions have several common options and some specific to their particular type.
See the [DSL documentation](https://hexdocs.pm/ash/dsl-ash-reactor.md) for
details.

## Handling failure.

Reactor is a saga executor, which means that when failure occurs it tries to
clean up any intermediate state left behind. By default the `create`, `update`
and `destroy` steps do not specify any behaviour for what to do when there is a
failure downstream in the reactor. This can be changed by providing both an
`undo_action` and changing the step's `undo` option to either
`:outside_transaction` or `:always` depending on your resource and datalayer
semantics.

### The `undo` option.

- `:never` - this is the default, and means that the reactor will never try and
  undo the action's work. This is the most performant option, as it means that
  the reactor doesn't need to store as many intermediate values.
- `:outside_transaction` - this option allows the step to decide at runtime
  whether it should support undo based on whether the action is being run within
  a transaction. If it is, then no undo is required because the transaction
  will rollback.
- `:always` - this forces the step to always undo it's work on failure.

### Transactions

You can use the `transaction` step type to wrap a group of steps inside a data layer transaction, however the following caveats apply:

- All steps inside a transaction must happen in the same process, so the steps
  inside the transaction will only ever be executed synchronously.
- Notifications will be sent only when the transaction is committed.

## Notifications

Because a reactor has transaction-like semantics notifications are automatically batched and only sent upon successful completion of the reactor.
