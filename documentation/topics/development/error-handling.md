# Error Handling

As of 3.0, Ash uses `Splode` to as our basis for errors. The documentation below still applies, but it is powered by `Splode` under the hood.

There is a difficult balance to cut between informative errors and enabling simple reactions to those errors. Since many extensions may need to work with and/or adapt their behavior based on errors coming from Ash, we need rich error messages. However, when you have a hundred different exceptions to represent the various kinds of errors a system can produce, it becomes difficult to say something like "try this code, and if it is invalid, do x, if it is forbidden, do y. To this effect, exceptions in Ash have one of four classes mapping to the top level exceptions.

## Error Classes

- forbidden - `Ash.Error.Forbidden`
- invalid - `Ash.Error.Invalid`
- framework - `Ash.Error.Framework`
- unknown - `Ash.Error.Unknown`

Since many actions can be happening at once, we want to support the presence of multiple errors as a result of a request to Ash. We do this by grouping up the errors into one before returning or raising.
We choose an exception based on the order of the exceptions listed above. If there is a single forbidden, we choose `Ash.Error.Forbidden`, if there is a single invalid, we choose `Ash.Error.Invalid` and so on. The actual errors will be included in the `errors` key on the exception. The exception's message will contain a bulleted list of all the underlying exceptions that occurred. This makes it easy to react to specific kinds of errors, as well as to react to _any/all_ of the errors present.

An example of a single error being raised, representing multiple underlying errors:

```elixir
AshExample.Representative
|> Ash.Changeset.for_create(:create, %{employee_id: "the best"})
|> Ash.create!()
 ** (Ash.Error.Invalid) Invalid Error
 * employee_id: must be absent.
 * first_name, last_name: at least 1 must be present.
```

This allows easy rescuing of the major error classes, as well as inspection of the underlying cases

```elixir
try do
  AshExample.Representative
  |> Ash.Changeset.for_create(:create, %{employee_id: "dabes"})
  |> Ash.create!()
rescue
  e in Ash.Error.Invalid ->
    "Encountered #{Enum.count(e.errors)} errors"
end

"Encountered 2 errors"
```

This pattern does add some additional overhead when you want to rescue specific kinds of errors. For example, you may need to do something like this:

```elixir
try do
  AshExample.Representative
  |> Ash.Changeset.for_create(:create, %{employee_id: "dabes"})
  |> Ash.create!()
rescue
  e in Ash.Error.Invalid ->
    case Enum.find(e.errors, &(&1.__struct__ == A.Specific.Error)) do
      nil ->
        ...handle errors
      error ->
        ...handle specific error you found
    end
end
```

## Generating Errors

When returning errors from behaviors or adding errors to a
changeset/query/action input, multiple formats are supported. You can return a
simple String, which will be converted into an `Ash.Error.Unknown` exception.
You can also return a keyword list containing `field` or `fields` and `message`,
which will be used to construct an `Ash.Error.Invalid.InvalidChanges` error.
Finally, you can pass an exception directly, which will be used as is if it is
an Ash error, or wrapped in an `Ash.Error.Unknown` if it is not.

Technically *any* value can be used as an error, but will be wrapped in an
`Ash.Error.Unknown` accordingly.

> ### Use exception modules {: .info}
>
> You should prefer to use the exception modules provided by Ash, or ones
> that you have defined manually. This allows you to document your error
> types, and to show those errors over API interfaces. See the section
> on APIs below for more.

## Examples of using non standard errors

### Keyword list (`Ash.Error.Changes.InvalidChanges`)

```elixir
def change(changeset, _, _) do
  if under_21?(changeset) do
    Ash.Changeset.add_error(changeset, field: :age, message: "must be 21 or older")
  else
    changeset
  end
end
```

### String (`Ash.Error.Unknown.UnknownError`)

```elixir
def change(changeset, _, _) do
  if under_21?(changeset) do
    Ash.Changeset.add_error(changeset, "must be 21 or older")
  else
    changeset
  end
end
```

## Using an exception module

These are all modules under `Ash.Error.*`. You can create a new one with `error.exception(options)`, and the options are documented in each exception. This documentation is missing in some cases. Go to the source code of the exception to see its special options. All of them support the `vars` option, which are values to be interpolated into the message, useful for things like translation.

For example:

```elixir
def change(changeset, _, _) do
  if under_21?(changeset) do
    error = Ash.Error.Changes.Required.exception(
      field: :foo,
      type: :attribute,
      resource: changeset.resource
    )

    Ash.Changeset.add_error(changeset, error)
  else
    changeset
  end
end
```

### Using a Custom Exception

You can create a custom exception like so. This is an example of a builtin exception that you could mirror to build your own

```elixir
defmodule MyApp.Errors.Invalid.TooYoung do
  @moduledoc "Used when a user who is too young is attempted to be created"
  use Splode.Error, fields: [:age], class: :invalid

  def message(error) do
    """
    Must be 21 or older, got: #{error.age}.
    """
  end
end

def change(changeset, _, _) do
  if under_21?(changeset) do
    error = MyApp.Errors.Invalid.TooYoung.exception(
      age: Ash.Changeset.get_attribute(changeset, :age)
    )

    Ash.Changeset.add_error(changeset, error)
  else
    changeset
  end
end
```

## Showing errors over APIs

AshJsonApi and AshGraphql both use a special protocol to determine how (and if) a raised or returned error should be displayed.

See the relevant docs:
- [handling errors in AshGraphql](https://hexdocs.pm/ash_graphql/handle-errors.html)
- [AshJsonApi.ToJsonApiError](https://hexdocs.pm/ash_json_api/AshJsonApi.ToJsonApiError.html)
