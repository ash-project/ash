<!--
SPDX-FileCopyrightText: 2024 splode contributors <https://github.com/ash-project/splode/graphs.contributors>

SPDX-License-Identifier: MIT
-->

# Get Started with Splode

Splode helps you deal with errors and exceptions in your application that are aggregatable and consistent. The general pattern is that you use the `Splode` module as a top level aggregator of error classes, and whenever you return errors, you return one of your `Splode.Error` structs, or a string, or a keyword list. Then, if you want to group errors together, you can use your `Splode` module to do so. You can also use that module to turn any arbitrary value into a splode error.

More documentation for `Splode` will come in the future. This was extracted from Ash Framework so that it could be standardized across multiple packages. If you use Ash, you can use `Ash.Errors` to get the benefits of `Splode`.

For now, here is an example:

```elixir
defmodule MyApp.Errors do
  use Splode, error_classes: [
    invalid: MyApp.Errors.Invalid,
    unknown: MyApp.Errors.Unknown
  ],
  unknown_error: MyApp.Errors.Unknown.Unknown
end

# Error classes are splode errors with an `errors` key.
defmodule MyApp.Errors.Invalid do
  use Splode.ErrorClass, class: :invalid
end

# You will want to define an unknown error class,
# otherwise splode will use its own
defmodule MyApp.Errors.Unknown do
  use Splode.ErrorClass, class: :unknown
end

# This fallback exception will be used for unknown errors
defmodule MyApp.Errors.Unknown.Unknown do
  use Splode.Error, class: :unknown

  # your unknown message should have an `error` key
  def message(%{error: error}) do
    if is_binary(error) do
      to_string(error)
    else
      inspect(error)
    end
  end
end

# Finally, you can create your own error classes

defmodule MyApp.Errors.InvalidArgument do
  use Splode.Error, fields: [:name, :message], class: :invalid

  def message(%{name: name, message: message}) do
    "Invalid argument #{name}: #{message}"
  end
end
```

To use these exceptions in your application, the general pattern is to return errors in `:ok | :error` tuples, like so:

```elixir
def do_something(argument) do
  if is_valid?(argument) do
    {:ok, do_stuff()}
  else
    {:error,
      MyApp.Errors.InvalidArgument.exception(
        name: :argument,
        message: "is invalid"
      )}
  end
end
```

Then, you can use `to_class`, and `to_error` tools to ensure that you have consistent error structures.

```elixir
def do_multiple_things(argument) do
  results = [do(), multiple(), things()]
  {results, errors} =
    Enum.reduce(results, {[], []}, fn
      {:ok, result}, {results, errors} ->
        {[result | results], errors}
      {:error, error} ->
        # ensure each error is a splode error
        # technically, `to_class` does this for you,
        # this is just an example
        {results, [MyApp.Errors.to_error(error) | errors]}
    end)

  case {results, errors} do
    {results, []} ->
      {:ok, results}
    {_results, errors} ->
      {:error, MyApp.Errors.to_class(errors)}
  end
end
```

## Error classes

When we combine errors into error classes, we choose the first error class for which there are any errors as the "class" of the combined error. For example, in Ash Framework, we have:

```elixir
use Splode,
  error_classes: [
    forbidden: Ash.Error.Forbidden,
    invalid: Ash.Error.Invalid,
    framework: Ash.Error.Framework,
    unknown: Ash.Error.Unknown
  ],
  unknown_error: Ash.Error.Unknown.UnknownError
```

What this means is that if there are any `Forbidden` errors, then the class is `Forbidden`. A `Forbidden` error _can_ contain any of the lower classed errors. This allows people to match on and/or rescue on "the general type of failure" that occurred. Given that you have many varied kinds of errors, you can use this to your advantage to have both detailed errors, but simple to match on errors. Here is an example:

```elixir
def get(conn, %{"user_id" => user_id}) do
  user = MyApp.Accounts.get_user!()
  render_user(conn, user)
rescue
  e in Ash.Error.Forbidden ->
    render_error(conn, %{error: "You can't do this"})

  e in Ash.Error.Invalid ->
    render_error(conn, %{error: "You did something wrong"})

  e in [Ash.Error.Framework, Ash.Error.Unknown] ->
    render_error(conn, %{error: "Something went wrong"})
end
```

Or, alternatively, you can pattern match on them given a non-raised error class

```elixir
def get(conn, %{"user_id" => user_id}) do
  case MyApp.Accounts.get_user() do
    {:ok, user} ->
      render_user(conn, user)

    {:error, %Ash.Error.Forbidden{}} ->
      render_error(conn, %{error: "You can't do this"})

    {:error, %Ash.Error.Invalid{}} ->
      render_error(conn, %{error: "You did something wrong"})

    {:error, %error{}} when error in [Ash.Error.Framework, Ash.Error.Unknown] ->
      render_error(conn, %{error: "Something went wrong"})
  end
end
```

# Raising Exceptions

To make a `!` version of a function, use `.unwrap!/2` on your splode module.

```elixir
def get_user!(user_id) do
  user_id
  |> get_user()
  |> MyApp.Errors.unwrap!()
end

def get_user(user_id) do
  case Repo.get(user_id) do
    nil ->
      {:error, MyApp.Error.NotFound.exception(resource: User, key: user_id)}
    user ->
      {:ok, user}
  end
end
```

## Installation

```elixir
def deps do
  [
    {:splode, "~> 0.1.0"}
  ]
end
```
