<!--
SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs.contributors>

SPDX-License-Identifier: MIT
-->

# Generic Actions

Generic actions are so named because there are no special rules about how they work. A generic action takes arguments and returns a value. The struct used for building input for a generic action is `Ash.ActionInput`.

```elixir
action :say_hello, :string do
  argument :name, :string, allow_nil?: false

  run fn input, _ ->
    {:ok, "Hello: #{input.arguments.name}"}
  end
end
```

A generic action declares its arguments, return type, and implementation, as illustrated above.

> ### No return? No problem! {: .tip}
>
> Generic actions can omit a return type, in which case running them returns `:ok` if successful.
>
> ```elixir
> action :schedule_job do
>   argument :job_name, :string, allow_nil?: false
>   run fn input, _ ->
>     # Schedule the job
>     :ok
>   end
> end
> ```

For a full list of all of the available options for configuring generic actions, see [the Ash.Resource.Dsl documentation](dsl-ash-resource.html#actions-action).

## Calling Generic Actions

The basic formula for calling a generic action looks like this:

```elixir
Resource
|> Ash.ActionInput.for_action(:action_name, %{argument: :value}, ...opts)
|> Ash.run_action!()
```

See the [code interface guide](/documentation/topics/code-interfaces.md) guide for how to
define idiomatic and convenient functions that call your actions.

## Why use generic actions?

The example above could be written as a normal function in elixir, i.e

```elixir
def say_hello(name), do: "Hello: #{name}"
```

The benefit of using generic actions instead of defining normal functions:

- They can be used with api extensions like `ash_json_api` and `ash_graphql`
- Their inputs are type checked and casted
- They support Ash authorization patterns (i.e policies)
- They can be included in the code interface of a resource
- They can be made transactional with a single option (`transaction? true`)

If you don't need any of the above, then there is no problem with writing regular Elixir functions!

## Return types and constraints

Generic actions do not cast their return types. It is expected that the action return a valid value for the type that they declare. However, declaring additional constraints can inform API usage, and make the action more clear. For example:

```elixir
action :priority, :integer do
  constraints [min: 1, max: 3]
  argument :status, :atom, constraints: [one_of: [:high, :medium, :low]]

  run fn input, _ ->
    case input.arguments.status do
      :high -> {:ok, 3}
      :medium -> {:ok, 2}
      :low -> {:ok, 1}
    end
  end
end
```

> #### Returning resource instances {: .info}
>
> It sometimes happens that you want to make a generic action which returns an
> instance or instances of the resource. It's natural to assume that you can
> set your action's return type to the name of your resource. This won't work
> as resources do not define a type, unless they are embedded. In embedded resources, this won't work because the module is still being compiled, so referencing yourself as a type causes a compile error. Instead, use the `:struct` type and the `instance_of` constraint, like so:
>
> ```elixir
> action :get, :struct do
>   constraints instance_of: __MODULE__
>
>   run # ...
> end
> ```
>
> For returning many instances of the resource, you can set your action's return type to
> `{:array, :struct}` and set the `items` constraint to the name of your resource.
>
> ```elixir
>  action :list_resources, {:array, :struct} do
>    constraints items: [instance_of: __MODULE__]
>
>    run # ...
>  end
> ```

## Calling Generic Actions

To execute a generic action in Ash, follow these steps:

1. **Prepare the action input:** Use `Ash.ActionInput.for_action/4` to specify the resource, the action and its arguments.
2. **Run the action:** Use `Ash.run_action/2` to execute the action with the prepared input.

### Example Usage

Consider an `Ash.Resource` with the action `:say_hello`:

```elixir
action :say_hello, :string do
  argument :name, :string, allow_nil?: false

  run fn input, _ ->
    {:ok, "Hello: #{input.arguments.name}"}
  end
end
```

Call this action:

```elixir
{:ok, greeting} = Resource
|> Ash.ActionInput.for_action(:say_hello, %{name: "Alice"})
|> Ash.run_action()

IO.puts(greeting)  # Output: Hello: Alice
```

### Using Code Interface

You can also use [Code Interfaces](documentation/topics/resources/code-interfaces.md) to call actions:

Given a definition like:

```elixir
define :say_hello, args: [:name]
```

```elixir
{:ok, greeting} = Resource.say_hello("Alice")
greeting = Resource.say_hello!("Alice")
```

## Validations and Preparations

Generic actions support validations and preparations, allowing you to add business logic and input validation to your actions.

### Validations

Validations in generic actions work similarly to those in other action types. They validate the action input before the action logic runs.

```elixir
action :create_user, :struct do
  constraints instance_of: __MODULE__

  argument :name, :string, allow_nil?: false
  argument :email, :string, allow_nil?: false
  argument :age, :integer

  validate present([:name, :email])
  validate match(:email, ~r/@/)
  validate compare(:age, greater_than: 13) do
    message "Must be at least 13 years old"
  end

  run fn input, _ ->
    # Create user logic here
    {:ok, %__MODULE__{
      name: input.arguments.name,
      email: input.arguments.email,
      age: input.arguments.age
    }}
  end
end
```

You can also use custom validation modules:

```elixir
action :transfer_funds, :boolean do
  argument :from_account, :string, allow_nil?: false
  argument :to_account, :string, allow_nil?: false
  argument :amount, :decimal, allow_nil?: false

  validate {MyApp.Validations.SufficientFunds, field: :amount}

  run fn input, _ ->
    # Transfer logic here
    {:ok, true}
  end
end
```

### Preparations

Preparations allow you to modify the action input before the action runs. This is useful for setting computed values or applying business logic.

```elixir
action :audit_log, :string do
  argument :action, :string, allow_nil?: false
  argument :details, :map, default: %{}

  prepare fn input, _context ->
    # Add timestamp and actor information
    updated_details = Map.merge(input.arguments.details, %{
      timestamp: DateTime.utc_now(),
      actor_id: input.context[:actor]&.id
    })

    Ash.ActionInput.set_argument(input, :details, updated_details)
  end

  run fn input, _ ->
    # Log the action
    log_entry = "#{input.arguments.action}: #{inspect(input.arguments.details)}"
    {:ok, log_entry}
  end
end
```

You can also use the built-in `build` preparation:

```elixir
action :search_with_defaults do
  argument :query, :string
  argument :filters, :map, default: %{}

  prepare build(
    arguments: %{
      filters: expr(Map.merge(^arg(:filters), %{active: true}))
    }
  )

  run fn input, _ ->
    # Search logic with default filters applied
    {:ok, perform_search(input.arguments.query, input.arguments.filters)}
  end
end
```

## Action Hooks

Generic actions support action-level hooks that run before and after the action execution.

### Before Action Hooks

Before action hooks run immediately before the action logic executes:

```elixir
action :process_payment, :boolean do
  argument :amount, :decimal, allow_nil?: false
  argument :payment_method, :string, allow_nil?: false

  validate present([:amount, :payment_method])

  # Using a function
  prepare before_action(fn input, _context ->
    # Log the payment attempt
    Logger.info("Processing payment of #{input.arguments.amount}")

    # Validate payment method
    if input.arguments.payment_method not in ["credit_card", "bank_transfer"] do
      Ash.ActionInput.add_error(input, "Invalid payment method")
    else
      input
    end
  end)

  run fn input, _ ->
    # Process payment logic
    {:ok, true}
  end
end
```

### After Action Hooks

After action hooks run after successful action execution:

```elixir
action :send_notification, :boolean do
  argument :message, :string, allow_nil?: false
  argument :recipient, :string, allow_nil?: false

  prepare after_action(fn input, result, _context ->
    # Log successful notification
    Logger.info("Notification sent to #{input.arguments.recipient}")

    # Could perform additional side effects here
    {:ok, result}
  end)

  run fn input, _ ->
    # Send notification logic
    send_notification(input.arguments.recipient, input.arguments.message)
    {:ok, true}
  end
end
```

### Using Custom Preparation Modules

You can also create reusable preparation modules for generic actions:

```elixir
defmodule MyApp.Preparations.AuditAction do
  use Ash.Resource.Preparation

  def prepare(input, _opts, context) do
    Ash.ActionInput.before_action(input, fn input ->
      # Log the action attempt
      MyApp.AuditLog.log_action(input.action.name, input.arguments, context.actor)
      input
    end)
    |> Ash.ActionInput.after_action(fn input, result ->
      # Log successful completion
      MyApp.AuditLog.log_success(input.action.name, result, context.actor)
      {:ok, result}
    end)
  end
end
```

Then use it in your action:

```elixir
action :sensitive_operation, :boolean do
  argument :data, :map, allow_nil?: false

  prepare MyApp.Preparations.AuditAction

  run fn input, _ ->
    # Sensitive operation logic
    {:ok, true}
  end
end
```

## Global Validations and Preparations

Generic actions also support global validations and preparations defined at the resource level:

```elixir
defmodule MyApp.MyResource do
  use Ash.Resource

  # Global preparations that apply to all actions
  preparations do
    prepare fn input, _context ->
      # Add tenant information to all actions
      Ash.ActionInput.set_context(input, %{tenant: "default"})
    end do
      # Only apply to generic actions
      on: [:action]
    end
  end

  # Global validations that apply to all actions
  validations do
    validate present(:actor) do
      message "Authentication required"
      on: [:action]  # Only apply to generic actions
    end
  end

  actions do
    action :my_action do
      # Action-specific logic
    end
  end
end
```

## Execution Order

For generic actions, the execution order is:

1. Global preparations/validations (in order of definition)
2. Action preparations/validations (in order of definition)
3. `before_action` hooks
4. Action logic execution
5. `after_action` hooks (success only)

This order ensures that global business logic runs first, followed by action-specific logic, and finally the action hooks.
