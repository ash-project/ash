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

## Example

An example is worth 1000 words of prose:

```elixir
defmodule ExampleReactor do
  use Ash.Reactor

  ash do
    default_domain ExampleDomain
  end

  input :customer_name
  input :customer_email
  input :plan_name
  input :payment_nonce

  create :create_customer, Customer do
    inputs %{name: input(:customer_name), email: input(:customer_email)}
  end

  read_one :get_plan, Plan, :get_plan_by_name do
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
See the [DSL documentation](dsl-ash-reactor.html) for
details.

### Action inputs

Ash actions take a map of input parameters which are usually a combination of
resource attributes and action arguments. You can provide these values as a
single map using the [`inputs` DSL entity](dsl-ash-reactor.html#reactor-action-inputs) with a map or keyword list which refers to Reactor inputs, results and hard-coded values via Reactor's [predefined template functions](https://hexdocs.pm/reactor/Reactor.Dsl.Argument.html#functions).

For action types that act on a specific resource (ie `update` and `destroy`) you can provide the value using the [`initial` DSL option](dsl-ash-reactor.html#reactor-update-initial).

#### Example

```elixir
input :blog_title
input :blog_body
input :author_email

read :get_author, MyBlog.Author, :get_author_by_email do
  inputs %{email: input(:author_email)}
end

create :create_post, MyBlog.Post, :create do
  inputs %{
    title: input(:blog, [:title]),
    body: input(:blog, [:body]),
    author_id: result(:get_author, [:email])
  }
end

update :author_post_count, MyBlog.Author, :update_post_count do
  wait_for :create_post
  initial result(:get_author)
end

return :create_post
```

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

### The `undo_action` option.

The behaviour of the `undo_action` is action specific:

- For `create` actions, the `undo_action` should be the name of a `destroy`
  action with no specific requirements.
- For `update` actions, the `undo_action` should also be an `update` action
  which takes a `changeset` argument, which will contain the `Ash.Changeset`
  which was used to execute the original update.
- For `destroy` actions, the `undo_action` should be the name of a `create`
  action which takes a `record` argument, which will contain the
  resource record which was used destroyed.

### Transactions

You can use the `transaction` step type to wrap a group of steps inside a data layer transaction, however the following caveats apply:

- All steps inside a transaction must happen in the same process, so the steps
  inside the transaction will only ever be executed synchronously.
- Notifications will be sent only when the transaction is committed.

## Notifications

Because a reactor has transaction-like semantics notifications are automatically batched and only sent upon successful completion of the reactor.

## Running Reactors as an action

Ash's [generic actions](actions.md#generic-actions) now support providing a Reactor module directly as their `run` option.

Notes:

- Every Reactor input must have a corresponding action argument.
- Ash's action context is passed in as the Reactor's context (including things like actor, tenant, etc).
- [Reactor runtime options](`t:Reactor.options/0`) can be set by setting `run {MyReactor, opts}` instead of just `run MyReactor`.
- If you set the `transaction?` action DSL option to true then the Reactor will be run synchronously - regardless of the value of the `async?` runtime option.

### Example

```elixir
action :run_reactor, :struct do
  constraints instance_of: MyBlog.Post

  argument :blog_title, :string, allow_nil?: false
  argument :blog_body, :string, allow_nil?: false
  argument :author_email, :ci_string, allow_nil?: false

  run MyBlog.CreatePostReactor
end
```
