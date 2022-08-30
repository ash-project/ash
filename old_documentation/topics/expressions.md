# Expressions

Ash has an "expression" syntax, which can be used in filters and calculations. More uses will likely be implemented in the future.

Expressions are not evaluated in-line. They are stored for later use and may be translated to SQL/Elixir to be executed. To include a variable somewhere in an expression, use a `^`, similar to how Ecto/pattern matching works. For example: `Ash.Query.filter(id == ^id)`. That would filter a resource to records where their id equals the variable `id`.

## Notice

The expression syntax is young, and in some cases may be missing basic operators/functions/syntax.
Please open a proposal issue in Ash. I generally release new versions of the project quickly, so if your proposal (and/or PR if you end up implementing the fix) will likely be released very soon. If you are using the `ash_postgres` datalayer, then you can often use `fragment/*` as an escape hatch. It works just like Ecto's `fragment`.

## Filters

`Ash.Query.filter/2` is a macro that accepts an expression by default. Here are some examples:

```elixir
# simple boolean operators
Ash.Query.filter(User, email == "foo@bar.com")

# simple function calls
Ash.Query.filter(User, is_nil(deactivated_at))

# boolean operators
Ash.Query.filter(User, is_nil(deactivated_at) or email == "foo@bar.com")

# using fragment with a field reference
search = "Bob Sagat"
Ash.Query.filter(User, fragment("levenshtein(?, ?)", first_name, ^search))

# referencing a related resource
Ash.Query.filter(User, profile.first_name == "Zach Daniel")
```

You can use expressions in the `filter` of a read action as well. There are two important differences between `Ash.Query.filter/2`:

* You have to call `Ash.Query.expr/1`, which is automatically imported in that context
* It is technically a filter *template*, which allows you to add some amount of dynamism to the expression, since it is statically embedded in the resource.

Filter templates support referencing fields on the actor, via `{:_actor, :field}`, arguments of the read action, via `{:_arg, :field}`, and values in the context, via `{:_context, :field}`. For readability, corresponding functions, `actor/1`, `arg/1`, and `context/1` that simply returns those values.

Here are some examples:

```elixir
read :current_user do
  filter expr(id == ^actor(:id))
end

read :by_id do
  argument :id, :uuid, allow_nil?: false

  filter expr(id == ^arg(:id))
end

read :active do
  filter expr(not(is_nil(activated_at)))
end
```

### Referencing related values

When referencing related values, if the reference is a `has_one` or `belongs_to`, the filter does exactly what it looks like (matches if the related value matches). If it is a `has_many` or a `many_to_many`, it matches if any of the related records match.

### Referencing aggregates and calculations

Aggregates are simple, insofar as all aggregates can be referenced in filter expressions (if you are using a data layer that supports it).

For calculations, only those that define an expression can be referenced in other expressions.

Here are some examples:

```elixir
# given a `full_name` calculation

Ash.Query.filter(User, full_name == "Hob Goblin")

# given a `full_name` calculation that accepts an argument called `delimiter`

Ash.Query.filter(User, full_name(delimiter: "~") == "Hob~Goblin")
```

## Calculations

There are two ways to make a calculation with an expression. The simplest, is to define the expression in-line with `expr/1`. The other is to use a custom `Ash.Calculation` module, and define an `expression/2` callback. This should return the expression that will ultimately be used. Doing this can allow you to define Elixir code that calculates the value of the expression (in `calculate/3`) as well. This means that, if the calculation is loaded but not referenced in a filter, sort or calculation, it can be calculated at runtime in Elixir. Eventually, logic will be added to support determining if an expression can be done at runtime, and this optimization will be added to inline `expr/1` calculations as well.

### Using `expr/1`

 Calculations can reference aggregates, other calculations, and attributes (but *not* relationships).
 Additionally, calculation expressions act as filter templates (see the filter template section above for more).

 For example:

```elixir
calculations do
  calculate :full_name, :string, expr(first_name <> " " <> last_name)
end
```

If you want to refer to a related value, you can use the `first` aggregate. For example to support referencing `first_name` and `last_name` on a user record where that information is stored in a related profile:

```elixir
aggregates do
  first :first_name, :profile, :first_name
  first :last_name, :profile, :last_name
end

calculations do
  calculate :full_name, :string, expr(first_name <> ^arg(:separator) <> last_name) do
    argument :separator, :string, default: " "
  end
end
```

As an aside: this also allows loading that value on the user, e.g `Ash.Query.load(User, [:first_name, :last_name])`

### Using calculation modules

An example calculation module to accomplish similar `concat` behavior as the examples above:

```elixir
defmodule MyApp.Calculations.Concat do
  @moduledoc false
  use Ash.Calculation
  require Ash.Query

  def init(opts) do
    if opts[:keys] && is_list(opts[:keys]) && Enum.all?(opts[:keys], &is_atom/1) do
      {:ok, opts}
    else
      {:error, "Expected a `keys` option for which keys to concat"}
    end
  end

  def select(_query, opts) do
    opts[:keys]
  end

  def expression(opts, _) do
    Enum.reduce(opts[:keys], nil, fn key, expr ->
      if expr do
        if opts[:separator] do
          Ash.Query.expr(expr <> ^opts[:separator] <> ref(^key))
        else
          Ash.Query.expr(expr <> ref(^key))
        end
      else
        Ash.Query.expr(ref(^key))
      end
    end)
  end

  def calculate(records, opts, _) do
    Enum.map(records, fn record ->
      Enum.map_join(opts[:keys], opts[:separator] || "", fn key ->
        to_string(Map.get(record, key))
      end)
    end)
  end
end
```

This can now be reused throughout your application, for example:

```elixir
alias MyApp.Calculations.Concat

calculations do
  calculate :full_name, {Concat, keys: [:first_name, :last_name, separator: " "]}
end
```
