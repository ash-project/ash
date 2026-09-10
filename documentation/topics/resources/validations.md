<!--
SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>

SPDX-License-Identifier: MIT
-->

# Validations

Validations are similar to [changes](/documentation/topics/resources/changes.md), except they cannot modify the changeset. They can only continue, or add an error.

Validations work on all action types. When used on queries and generic actions, they validate the arguments to ensure they meet your requirements before processing.

## Builtin Validations

There are a number of builtin validations that can be used, and are automatically imported into your resources. See `Ash.Resource.Validation.Builtins` for more.

### Query Support

The following builtin validations support both changesets and queries:
- `action_is` - validates the action name
- `argument_does_not_equal`, `argument_equals`, `argument_in` - validates argument values
- `compare` - compares values (arguments or attributes)
- `confirm` - confirms two values match
- `match` - validates values against regex patterns
- `negate` - negates other validations
- `one_of` - validates values are in allowed options
- `present` - validates required values are present
- `byte_size` - validates string byte size
- `string_length` - validates string length

Some examples of usage of builtin validations

```elixir
# Works on both changesets and queries
validate match(:email, "@")

validate compare(:age, greater_than_or_equal_to: 18) do
  message "must be over 18 to sign up"
end

validate byte_size(:password, max: 72)

validate present(:last_name) do
  where [present(:first_name), present(:middle_name)]
  message "must also be supplied if setting first name and middle_name"
end

# Example for read actions
actions do
  read :search do
    argument :email, :string
    argument :role, :string
    
    validate match(:email, ~r/^[^\s]+@[^\s]+\.[^\s]+$/)
    validate one_of(:role, ["admin", "user", "moderator"])
  end
  
  # Example for generic actions
  action :send_notification, :boolean do
    argument :recipient_email, :string
    argument :priority, :atom
    
    validate match(:recipient_email, ~r/^[^\s]+@[^\s]+\.[^\s]+$/)
    validate one_of(:priority, [:low, :medium, :high])
  end
end
```

## Custom Validations

```elixir
defmodule MyApp.Validations.IsPrime do
  # transform and validate opts

  use Ash.Resource.Validation

  @impl true
  def init(opts) do
    if opts[:field] != nil && is_atom(opts[:field]) do
      {:ok, opts}
    else
      {:error, "field must be an atom!"}
    end
  end

  @impl true
  def supports(_opts), do: [Ash.Changeset]

  @impl true
  def validate(changeset, opts, _context) do
    value = Ash.Changeset.get_attribute(changeset, opts[:field])
    # this is a function I made up for example
    if is_nil(value) || Math.is_prime?(value) do
      :ok
    else
      # The returned error will be passed into `Ash.Error.to_ash_error/3`
      {:error, field: opts[:field], message: "must be prime"}
    end
  end
end
```

### Supporting Queries in Custom Validations

To make a custom validation work on both changesets and queries, implement the `supports/1` callback:

```elixir
defmodule MyApp.Validations.ValidEmail do
  use Ash.Resource.Validation

  @impl true
  def init(opts) do
    {:ok, opts}
  end

  @impl true
  def supports(_opts), do: [Ash.Changeset, Ash.Query]

  @impl true
  def validate(subject, opts, _context) do
    value = get_value(subject, opts[:field])
    
    if is_nil(value) || valid_email?(value) do
      :ok
    else
      {:error, field: opts[:field], message: "must be a valid email"}
    end
  end

  defp get_value(%Ash.Changeset{} = changeset, attribute) do
    Ash.Changeset.get_argument_or_attribute(changeset, attribute)
  end

  defp get_value(%Ash.Query{} = query, attribute) do
    Ash.Query.get_argument(query, attribute)
  end

  defp valid_email?(email) do
    String.match?(email, ~r/^[^\s]+@[^\s]+\.[^\s]+$/)
  end
end
```

This could then be used in a resource via:

```elixir
validate {MyApp.Validations.IsPrime, attribute: :foo}
```

## Anonymous Function Validations

You can also use anonymous functions for validations. Keep in mind, these cannot be made atomic. This is great for prototyping, but we generally recommend using a module, both for organizational purposes, and to allow adding atomic behavior.

```elixir
validate fn changeset, _context ->
  # put your code here
end
```

## Where

The `where` can be used to perform validations conditionally.

The value of the `where` option can either be a validation or a list of validations. All of the `where`-validations must first pass for the main validation to be applied. For expressing complex conditionals, passing a list of built-in validations to `where` can serve as an alternative to writing a custom validation module.

### Examples

```elixir
validate present(:other_number), where: absent(:that_number)
```

```elixir
validate present(:other_number) do
  where {MyApp.Validations.IsPrime, attribute: :foo}
end
```

```elixir
validate present(:other_number),
  where: [
    numericality(:large_number, greater_than: 100),
    one_of(:magic_number, [7, 13, 123])
  ]
```

## Action vs Global Validations

You can place a validation in any create, update, or destroy action. For example:

```elixir
actions do
  create :create do
    validate compare(:age, greater_than_or_equal_to: 18)
  end
end
```

Or you can use the global validations block to validate on all actions of a given type. Where statements can be used in either. Note the warning about running on destroy actions below.

```elixir
validations do
  validate present([:foo, :bar], at_least: 1) do
    on [:create, :update]
    where present(:baz)
  end
end
```

The validations section allows you to add validations across multiple actions of a changeset

> ### Running on destroy actions {: .warning}
>
> By default, validations in the global `validations` block will run on create and update only. Many validations don't make sense in the context of destroys. To make them run on destroy, use `on: [:create, :update, :destroy]`

## only_when_valid? Option

Use the `only_when_valid?` option to skip validations when the changeset or query is already invalid. This is useful for expensive validations that should only run if other validations have passed.

```elixir
actions do
  create :create do
    validate present(:required_field)
    
    # This expensive validation only runs if query is valid so far
    validate expensive_external_validation() do
      only_when_valid? true
    end
  end
  
  read :search do
    argument :email, :string
    
    validate present(:email)
    
    # Only validate email format if email is present
    validate match(:email, ~r/^[^\s]+@[^\s]+\.[^\s]+$/) do
      only_when_valid? true
    end
  end
end
```

### Examples

```elixir
validations do
  validate present([:foo, :bar]), on: :update
  validate present([:foo, :bar, :baz], at_least: 2), on: :create
  validate present([:foo, :bar, :baz], at_least: 2), where: [action_is([:action1, :action2])]
  validate absent([:foo, :bar, :baz], exactly: 1), on: [:update, :destroy]
  validate {MyCustomValidation, [foo: :bar]}, on: :create
end
```

## before_action? Option

By default, validations run while the changeset, query, or action input is being *built*, i.e. inside `Ash.Changeset.for_create/4`, `Ash.Changeset.for_update/4`, `Ash.Query.for_read/4` and friends. That happens before the action is executed and outside of any transaction. Setting `before_action? true` defers a validation into a `before_action` hook instead, so it runs when the action is actually executed with `Ash.create/2`, `Ash.update/2`, etc.

```elixir
actions do
  update :assign_worker do
    argument :worker_id, :uuid, allow_nil?: false

    # Runs a query, so only run it on execution, inside the transaction
    validate MyApp.Validations.WorkerIsActive do
      before_action? true
      only_when_valid? true
    end
  end
end
```

There are two reasons to do this:

1. **The validation is expensive, and changesets are built more often than they are executed.** A validation that runs a query or calls an external service will otherwise run every time the changeset is built. For example, `AshPhoenix.Form.validate/3` rebuilds the changeset on every form change event, so a querying validation would hit the database on every keystroke. With `before_action? true` it only runs on submit.

2. **The validation needs to see the state at execution time.** `before_action` hooks run *inside* the data layer transaction, immediately before the data layer operation. A check made at build time can be stale by the time the action runs. Running inside the transaction means the validation observes the same state as the write that follows it. Note that this does not by itself lock rows, so for guarantees like uniqueness you still want an identity or a database constraint.

Things to keep in mind:

- **Errors surface later.** A `before_action?` validation is not run by `Ash.Changeset.for_create/4` and friends, so its errors will not be shown during live form validation, only once the action is submitted.
- **It cannot be atomic.** Because it runs Elixir code inside a hook, an update or destroy action with `require_atomic? true` will fail with an error explaining this. Bulk actions fall back to the `:stream` strategy. Set `require_atomic? false` on the action if you need this.
- **It runs after `before_transaction` and `around_transaction` hooks.** Those hooks run outside the transaction, before any `before_action?` validation. See the action lifecycle in `d:Ash.Resource.Dsl.actions` for the full hook order.
- **`only_when_valid?` is checked at hook time.** Combining both options, as in the example above, skips the expensive check if any earlier validation or change has already added an error.
- **`delay_global_validations?`** on an action is equivalent to setting `before_action? true` on every validation from the global `validations` block for that action.

## Atomic Validations

To make a validation atomic, you have to implement the `c:Ash.Resource.Validation.atomic/3` callback. This callback returns an atomic instruction, or a list of atomic instructions, or an error/indication that the validation cannot be done atomically. For our `IsPrime` example above, this would look something like:

```elixir
defmodule MyApp.Validations.IsPrime do
  # transform and validate opts

  use Ash.Resource.Validation

  ...

  def atomic(changeset, opts, context) do
    # lets ignore that there is no easy/built-in way to check prime numbers in postgres
    {:atomic,
      # the list of attributes that are involved in the validation
      [opts[:field]],
      # the condition that should cause the error
      # here we refer to the new value or the current value
      expr(not(fragment("is_prime(?)", ^atomic_ref(opts[:field])))),
      # the error expression
      expr(
        error(^InvalidAttribute, %{
          field: ^opts[:field],
          # the value that caused the error
          value: ^atomic_ref(opts[:field]),
          # the message to display
          message: ^(context.message || "%{field} must be prime"),
          vars: %{field: ^opts[:field]}
        })
      )
    }
  end
end
```

In some cases, validations operate on arguments only and therefore have no need of atomic behavior. for this, you can call `validate/3` directly from `atomic/3`. The builtin `Ash.Resource.Validation.Builtins.argument_equals/2` validation does this, for example.

```elixir
@impl true
def atomic(changeset, opts, context) do
  validate(changeset, opts, context)
end
```
