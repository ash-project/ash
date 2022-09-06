# Expressions

Ash expressions are used in various places like calculations, filters, and policies, and are meant to be portable representations of elixir expressions. You can create an expression using the `Ash.Query.expr/1` macro, like so:

```elixir
Ash.Query.expr(1 + 2)
Ash.Query.expr(x + y)
Ash.Query.expr(post.title <> " | " <> post.subtitle)
```

Ash expressions have some interesting properties in their evaluation, primarily because they are made to be portable, i.e executable in some data layer (like SQL) or executable in Elixir. In general, these expressions will behave the same way they do in Elixir. The primary difference is how `nil` values work. They behave the way that `NULL` values behave in SQL. This is primarily because this pattern is easier to replicate to various popular data layers, and is generally safer when using expressions for things like authentication. The practical implications of this are that `nil` values will "poison" many expressions, and cause them to return `nil`. For example, `x + nil` would always evaluate to `nil`.

## Operators

The following operators are available and they behave the same as they do in Elixir, except for the `nil` addendum above.

- `==`
- `!=`
- `>`
- `>=`
- `<`
- `<=`
- `in`
- `*`
- `-`
- `/`
- `<>`
- `||`
- `&&`
- `is_nil` | Custom, accepts a boolean on the right side i.e `x is_nil true` or `x is_nil false`.

## Functions

The following functions are built in. Data Layers can add their own functions to expressions. For example, `AshPostgres` adds a `fragment` function that allows you to provide SQL directly.

The following functions are built in:

- `if` | Works like elixir's `if`.
- `is_nil` | Works like elixir's `is_nil`
- `get_path` | i.e `get_path(value, ["foo", "bar"])`. This is what expressions like `value[:foo]["bar"]` are turned into under the hood.
- `ago` | i.e `deleted_at > ago(7, :day)`. The available time intervals are documented in {{link:ash:module:Ash.Type.DurationName}}
- `contains` | if one string contains another string, i.e `contains("fred", "red")`

## Primitives

- `cond` - `cond` is transformed to a series of `if` expressions under the hood
- `item[:key] or item["key"]` - accesses keys in a map. In both cases, it prefers a matching atom key, falling back to a matching string key. This is to aid with data stores that store embeds as JSON with string keys (like AshPostgres), so that this expression behaves the same in the data layer as it does in Elixir.

## Templates

Most of the time, when you are using an expression, you will actually be creating a `template`. In this template, you have a few references that can be used, which will be replaced when before the expression is evaluated. The following references are available:

```elixir
actor(:key) # equivalent to `get_in(actor || %{}, [:key])`
actor([:key1, :key2]) # equivalent to `get_in(actor || %{}, [:key, :key2])`
arg(:arg_name) # equivalent to `Map.get(arguments, :arg_name)`
ref(:key) # equivalent to referring to `key`. Allows for dynamic references
ref(:key, [:path]) # equivalent to referring to `path.key`. Allows for dynamic references with dynamic (or static) paths.
context(:key) # equivalent to `get_in(context, :key)`
context([:key1, :key2]) # equivalent to `get_in(context, [:key1, :key2])`
```

## Use cases for expressions

### Filters

The most obvious place we use expressions is when filtering data. For example:

```elixir
Ash.Query.filter(Ticket, status == :open and opened_at >= ago(10, :day))
```

These filters will be run in the data layer, i.e in the SQL query.

## Portability

Ash expressions being portable is more important than it sounds. For example, if you were using AshPostgres and had the following calculation, which is an expression capable of being run in elixir or translated to SQL:

```elixir
calculate :full_name, :string, expr(first_name <> " " <> last_name)
```

And you did something like the following:

```elixir
User
|> Ash.Query.load(:full_name)
|> Ash.Query.sort(:full_name)
|> Accounts.read!()
```

You would see that it ran a SQL query with the `full_name` calculation as SQL. This allows for sorting on that value. However, if you had something like this:

```elixir
# data can be loaded in the query like above, or on demand later
Accounts.load!(user, :full_name)
```

you would see that no SQL queries are run. The calculation is simply run in Elixir and the value is set.

### Referencing related values

Related values can be references using dot delimiters, i.e `Ash.Query.filter(user.first_name == "fred")`.
When referencing related values in filters, if the reference is a `has_one` or `belongs_to`, the filter does exactly what it looks like (matches if the related value matches). If it is a `has_many` or a `many_to_many`, it matches if any of the related records match.

### Referencing aggregates and calculations

Aggregates are simple, as all aggregates can be referenced in filter expressions (if you are using a data layer that supports it).

For calculations, only those that define an expression can be referenced in other expressions.

Here are some examples:

```elixir
# given a `full_name` calculation

Ash.Query.filter(User, full_name == "Hob Goblin")

# given a `full_name` calculation that accepts an argument called `delimiter`

Ash.Query.filter(User, full_name(delimiter: "~") == "Hob~Goblin")
```