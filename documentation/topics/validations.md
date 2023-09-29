# Validations

## Builtin Validations

Checkout the documentation for `Ash.Resource.Validation.Builtins` to see the builtin validations.

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
  def init(opts) do
    if is_atom(opts[:attribute]) do
      {:ok, opts}
    else
      {:error, "attribute must be an atom!"}
    end
  end

  def validate(changeset, opts) do
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

## Where

The `where` can be used to perform changes/validations conditionally. This functions by running the validation, and if the validation returns an error, we discard the error and skip the operation. This means that even custom validations can be used in conditions.

For example:

```elixir
validate present(:other_number) do
  where [{MyApp.Validations.IsPrime, attribute: :foo}]
end
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

Or you can use the global validations block to validate on all actions of a given type. Where statements can be used in either. Use `on` to determine the types of actions the validation runs on. By default, it only runs on create an update actions

```elixir
validations do
  validate present([:foo, :bar], at_least: 1) do
    on [:create, :update]
    where present(:baz)
  end
end
```

## Action-Specific Validation

You can also put a validation directly in an action, like so:

```elixir
actions do
  create do
    ...
    validate present([:foo, :bar], at_least: 1)
  end
end
```
