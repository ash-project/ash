# Calculations

Calculations in Ash allow for displaying complex values as a top level value of a resource.
They are relatively limited in their current form, supporting only functional calculations,
where you provide a module that takes a list of records and returns a list of values for that
calculation. Eventually, there will be support for calculations that can be embedded into the
data layer(for things like postgres) that will allow for sorting and filtering on calculated
data.

## Declaring calculations on a resource

Example:

```elixir
defmodule Concat do
  # An example concatenation calculation, that accepts the delimeter as an argument,
  #and the fields to concatenate as options
  use Ash.Calculation, type: :string

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
end

# Usage in a resource
calculations do
  calculate :full_name, {Concat, keys: [:first_name, :last_name]} do
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
|> Ash.Query.calculate(:full_name, {Concat, keys: [:first_name, :last_name]}, %{separator: ","})
```

See the documentation for `Ash.Query.calculate/4` for more information.
