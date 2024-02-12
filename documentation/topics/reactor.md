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

## Notifications

Because a reactor has transaction-like semantics notifications are automatically batched and only sent upon successful completion of the reactor.
