# Calculations

Calculations in Ash allow for displaying complex values as a top level value of a resource.

## Declaring calculations on a resource

### Expression Calculations

The simplest kind of calculation refers to an Ash expression. For example:

```elixir
calculations do
  calculate :full_name, :string, expr(first_name <> " " <> last_name)
end
```

See the [Expressions guide](/documentation/topics/expressions.md) for more.

### Module Calculations

When calculations require more complex code or can't be pushed down into the data layer, a module that uses `Ash.Calculation` can be used.

```elixir
defmodule Concat do
  # An example concatenation calculation, that accepts the delimiter as an argument,
  #and the fields to concatenate as options
  use Ash.Calculation

  # Optional callback that verifies the passed in options (and optionally transforms them)
  @impl true
  def init(opts) do
    if opts[:keys] && is_list(opts[:keys]) && Enum.all?(opts[:keys], &is_atom/1) do
      {:ok, opts}
    else
      {:error, "Expected a `keys` option for which keys to concat"}
    end
  end

  @impl true
  def calculate(records, opts, %{separator: separator}) do
    Enum.map(records, fn record ->
      Enum.map_join(opts[:keys], separator, fn key ->
        to_string(Map.get(record, key))
      end)
    end)
  end

  # You can implement this callback to make this calculation possible in the data layer
  # *and* in elixir. Ash expressions are already executable in Elixir or in the data layer, but this gives you fine grain control over how it is done
  # @impl true
  # def expression(opts, context) do
  # end
end

# Usage in a resource
calculations do
  calculate :full_name, :string, {Concat, keys: [:first_name, :last_name]} do
    # You currently need to use the [allow_empty?: true, trim?: false] constraints here.
    # The separator could be an empty string or require a leading or trailing space, 
    # but would be trimmed or even set to `nil` without the constraints.
    argument :separator, :string, constraints: [allow_empty?: true, trim?: false]
  end
end
```

See the documentation for the calculations section in `Ash.Resource.Dsl` and the `Ash.Calculation` docs for more information.

The calculations declared on a resource allow for declaring a set of named calculations that can be used by extensions.
They can also be loaded in the query using `Ash.Query.load/2`, or after the fact using `c:Ash.Api.load/3`. Calculations declared on the resource will be keys in the resource's struct.

## Custom calculations in the query

Example:

```elixir
User
|> Ash.Query.new()
|> Ash.Query.calculate(:full_name, {Concat, keys: [:first_name, :last_name]}, :string, %{separator: ","})
```

See the documentation for `Ash.Query.calculate/4` for more information.

## Arguments in calculations

Using the above example with arguments, you can load a calculation with arguments like so:

```elixir
load(full_name: [separator: ","])
```

If the calculation uses an expression, you can also filter and sort on it like so:

```elixir
query
|> Ash.Query.filter(full_name(separator: ","))
|> Ash.Query.sort(full_name: {:asc, %{separator: ","}})
```

## Async loading

Expensive calculations can be marked as `allow_async?: true`, which will allow Ash to fetch it after the main query is run, in parallel with any other calculations that are being run async. This won't affect calculations that are being filtered on, since that must be placed in the data layer.
