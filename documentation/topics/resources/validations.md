# Validations

Validations are similar to [changes](/documentation/topics/resources/changes.md), except they cannot modify the changeset. They can only continue, or add an error.

## Builtin Validations

There are a number of builtin validations that can be used, and are automatically imported into your resources. See `Ash.Resource.Validation.Builtins` for more.

Some examples of usage of builtin validations

```elixir
validate match(:email, ~r/@/)

validate compare(:age, greater_than_or_equal_to: 18) do
  message "must be over 18 to sign up"
end

validate present(:last_name) do
  where [present(:first_name), present(:middle_name)]
  message "must also be supplied if setting first name and middle_name"
end
```

## Custom Validations

```elixir
defmodule MyApp.Validations.IsPrime do
  # transform and validate opts

  use Ash.Resource.Validation

  @impl true
  def init(opts) do
    if is_atom(opts[:attribute]) do
      {:ok, opts}
    else
      {:error, "attribute must be an atom!"}
    end
  end

  @impl true
  def validate(changeset, opts, _context) do
    value = Ash.Changeset.get_attribute(changeset, opts[:attribute])
    # this is a function I made up for example
    if is_nil(value) || Math.is_prime?(value) do
      :ok
    else
      # The returned error will be passed into `Ash.Error.to_ash_error/3`
      {:error, field: opts[:attribute], message: "must be prime"}
    end
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

### Examples

```elixir
validations do
  validate present([:foo, :bar]), on: :update
  validate present([:foo, :bar, :baz], at_least: 2), on: :create
  validate present([:foo, :bar, :baz], at_least: 2), where: [action_is(:action1, :action2)]
  validate absent([:foo, :bar, :baz], exactly: 1), on: [:update, :destroy]
  validate {MyCustomValidation, [foo: :bar]}, on: :create
end
```

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
      [opts[:attribute]],
      # the condition that should cause the error
      # here we refer to the new value or the current value
      expr(not(fragment("is_prime(?)", ^atomic_ref(opts[:attribute])))),
      # the error expression
      expr(
        error(^InvalidAttribute, %{
          field: ^opts[:attribute],
          # the value that caused the error
          value: ^atomic_ref(opts[:attribute]),
          # the message to display
          message: ^(context.message || "%{field} must be prime"),
          vars: %{field: ^opts[:attribute]}
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
