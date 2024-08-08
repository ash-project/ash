# Expressions

Ash expressions are used in various places like calculations, filters, and policies, and are meant to be portable representations of elixir expressions. You can create an expression using the `Ash.Expr.expr/1` macro, like so:

```elixir
Ash.Expr.expr(1 + 2)
Ash.Expr.expr(x + y)
Ash.Expr.expr(post.title <> " | " <> post.subtitle)
```

> ### Ash Expressions are SQL-ish {: .info}
>
> Ash expressions have some interesting properties in their evaluation, primarily because they are made to be
> portable, i.e executable in some data layer (like SQL) or executable in Elixir. In general, these expressions
> will behave the same way they do in Elixir. The primary difference is how `nil` values work. They behave the way
> that `NULL` values behave in SQL. This is primarily because this pattern is easier to replicate to various popular
> data layers, and is generally safer when using expressions for things like authentication. The practical
> implications of this are that `nil` values will "poison" many expressions, and cause them to return `nil`.
> For example, `x + nil` would always evaluate to `nil`. Additionally, `true and nil` will always result in
> `nil`, _this is also true with or and not_, i.e `true or nil` will return `nil`, and `not nil` will return `nil`.
>
> Additionally, atoms and strings compare as if the atom was a string. This is because most external data layers
> do not know about atoms, and so they are converted to strings before comparison.

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
- `is_nil` | Only works as an operator in maps/keyword list syntax. i.e `[x: [is_nil: true]]`

## Functions

The following functions are built in. Data Layers can add their own functions to expressions. For example, `AshPostgres` adds `trigram_similarity` function.

The following functions are built in:

- `if` | Works like elixir's `if`.
- `is_nil/1` | Works like elixir's `is_nil`
- `get_path/2` | i.e `get_path(value, ["foo", "bar"])`. This is what expressions like `value[:foo]["bar"]` are turned into under the hood.
- `contains/2` | if one string contains another string, i.e `contains("fred", "red")`
- `length/1` | the length of a list, i.e. `length([:foo, :bar])`
- `type/2` | Cast a given value to a specific type, i.e `type(^arg(:id), :uuid)` or `type(integer_field, :string)`
- `string_downcase/1` | Downcases a string
- `string_join/1` | Concatenates a list of strings, and ignores any nil values
- `string_join/2` | As above, but with a joiner
- `string_split/1` | Splits a string on spaces
- `string_split/2` | As above, but with a specific delimiter
- `string_split/3` | As above, but with options. See the function for the available options.
- `string_length/1` | Returns the length of a given string, as reported by `String.length/1`
- `string_trim/1` | Trims unicode whitespace from the beginning and the end of a string
- `at/2` | Get an element from a list, i.e `at(list, 1)`
- `round/1` | Round a float, decimal or int to 0 precision, i.e `round(num)`
- `round/2` | Round a float, decimal or int to the provided precision or less, i.e `round(1.1234, 3) == 1.1234` and `round(1.12, 3) == 1.12`
- String interpolation | `"#{first_name} #{last_name}"`, is remapped to the equivalent usage of `<>`
- `fragment/*` | Creates a fragment of a data layer expression. See the section on fragments below.

## Fragments

Fragments come in two forms.

## String Fragments

For SQL/query-backed data layers, they will be a string with question marks for interpolation. For example: `fragment("(? + ?)", foo, bar)`.

## Function Fragments

For elixir-backed data layers, they will be a function or an MFA that will be called with the provided arguments. For example: `fragment(&Module.add/2, foo, bar)` or `fragment({Module, :add, []}, foo, bar)`. When using anonymous functions, you can _only_ use the format `&Module.function/arity`. `&Module.add/2` is okay, but `fn a, b -> Module.add(a, b) end` is not.

## Sub-expressions

- `exists/2` | `exists(foo.bar, name == "fred")` takes an expression scoped to the destination resource, and checks if any related entry matches. See the section on `exists` below.
- `path.exists/2` | Same as `exists` but the source of the relationship is itself a nested relationship. See the section on `exists` below.
- `parent/1` | Allows an expression scoped to a resource to refer to the "outer" context. Used in relationship filters and `exists`

## DateTime Functions

- `now/0` | Evaluates to the current time when the expression is evaluated
- `today/0` | Evaluates to the current date when the expression is evaluated
- `ago/2` | i.e `deleted_at > ago(7, :day)`. The available time intervals are documented in `Ash.Type.DurationName`
- `from_now/2` | Same as `ago` but adds instead of subtracting
- `datetime_add/3` | add an interval to a datetime, i.e `datetime_add(^datetime, 10, :hour)`
- `date/3` | add an interval to a date, i.e `datetime_add(^date, 3, :day)`

## Primitives

- `cond` - `cond` is transformed to a series of `if` expressions under the hood
- `item[:key] or item["key"]` - accesses keys in a map. In both cases, it prefers a matching atom key, falling back to a matching string key. This is to aid with data stores that store embeds as JSON with string keys (like AshPostgres), so that this expression behaves the same in the data layer as it does in Elixir.

## Escape Hatches

- `lazy/1` - Takes an MFA and evaluates it just before running the query. This is important for calculations, because the `expression/2` callback should be _stable_ (returns the same value given the same input). For example `lazy({ULID, :generate, [timestamp_input]})`

## Inline Aggregates

Aggregates can be referenced in-line, with their relationship path specified and any options provided that match the options given to `Ash.Query.Aggregate.new/4`. For example:

```elixir
calculate :grade, :decimal, expr(
  count(answers, query: [filter: expr(correct == true)]) /
  count(answers, query: [filter: expr(correct == false)])
)
```

The available aggregate kinds can also be seen in the `Ash.Query.Aggregate` module documentation.

## Templates

Most of the time, when you are using an expression, you will actually be creating a `template`. In this template, you have a few references that can be used, which will be replaced when before the expression is evaluated. The following references are available.

```elixir
^actor(:key) # equivalent to `get_in(actor || %{}, [:key])`
^actor([:key1, :key2]) # equivalent to `get_in(actor || %{}, [:key, :key2])`
^arg(:arg_name) # equivalent to `Map.get(arguments, :arg_name)`
^context(:key) # equivalent to `get_in(context, :key)`
^context([:key1, :key2]) # equivalent to `get_in(context, [:key1, :key2])`
^ref(:key) # equivalent to referring to `key`. Allows for dynamic references
^ref([:path], :key) # equivalent to referring to `path.key`. Allows for dynamic references with dynamic (or static) paths.
```

## Custom Expressions

Custom expressions allow you to extend Ash's expression language with your own expressions. To see more, see `Ash.CustomExpression`. To add a custom expression, configure it and recompile ash. For example:

```elixir
config :ash, :custom_expressions, [
  MyApp.CustomExpression
]
```

```
mix deps.compile ash --force
```

These expressions will be available across all usages of Ash expressions within your application.

## Filter semantics & joins

The semantics of Ash filters are probably slightly different than what you are used to, and they are important to understand. Every filter expression is always talking about a single row, potentially "joined" to single related rows. By referencing relationships, you are implicitly doing a join. For those familiar with SQL terminology, it is equivalent to a left join, although AshPostgres can detect when it is safe to do an inner join (for performance reasons). Lets use an example of `posts` and `comments`.

Given a filter like the following:

```elixir
Ash.Query.filter(Post, comments.points > 10 and comments.tag.name == "elixir")
```

The filter refers to a _single post/comment/tag combination_. So in english, this is "posts where they have a comment with more than 10 points and _that same comment_ has a tag with the name `elixir`". What this also means is that filters like the above do not compose nicely when new filters are added. For example:

```elixir
def has_comment_with_more_points_than(query, score) do
  Ash.Query.filter(query, comments.points > ^score)
end

def has_comment_tagged(query, tag) do
  Ash.Query.filter(query, comments.tag.name == ^tag)
end

Post
|> has_comment_with_more_points_than(10)
|> has_comment_tagged("elixir")
```

That code _seems_ like it ought to produce a filter over `Post` that would give us any post with a comment having more than 10 points, _and_ with a comment tagged `elixir`. That is not the same thing as having a _single_ comment that meets both those criteria. So how do we make this better?

### Exists

Lets rewrite the above using exists:

```elixir
def has_comment_with_more_points_than(query, score) do
  Ash.Query.filter(query, exists(comments, points > ^score))
end

def has_comment_tagged(query, tag) do
  Ash.Query.filter(query, exists(comments.tag.name == ^tag)
end

Post
|> has_comment_with_more_points_than(10)
|> has_comment_tagged("elixir")
```

Now, they will compose properly! Generally speaking, you should use exists when you are filtering across any relationships that are `to_many` relationships \*even if you don't expect your filter to be composed. Currently, the filter syntax does not minimize(combine) these `exists/2` statements, but doing so is not complex and can be added. While unlikely, please lodge an issue if you see any performance issues with `exists`.

### Exists at path

Sometimes, you want the ability to say that some given row must have an existing related entry matching a filter. For example:

```elixir
Ash.Query.filter(Post, author.exists(roles, name == :admin) and author.active)
```

While the above is not common, it can be useful in some specific circumstances, and is used under the hood by the policy authorizer when combining the filters of various resources to create a single filter.

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
Accounts.load!(user, :full_name, reuse_values?: true)
```

you would see that no SQL queries are run. The calculation is run directly in Elixir without needing to visit the database.

## Parent

`Parent` is a way to "jump out" of a scoped expression. Here are some examples:

```elixir
Ash.Query.filter(exists(open_tickets, severity >= parent(severity_threshold)))
```

```elixir
has_many :relevant_tickets, Ticket do
  no_attributes? true
  # this says that there is no matching source_attribute and destination_attribute on this relationship
  filter expr(status == :open and severity >= parent(severity_threshold))
end
```

```elixir
count :count_of_relevant_tickets, :open_tickets do
  filter expr(status == :open and severity >= parent(severity_threshold))
end
```

### Referencing related values

Related values can be references using dot delimiters, i.e `Ash.Query.filter(user.first_name == "fred")`.
When referencing related values in filters, if the reference is a `has_one` or `belongs_to`, the filter does exactly what it looks like (matches if the related value matches). If it is a `has_many` or a `many_to_many`, it matches if any of the related records match.

### Referencing aggregates and calculations

Aggregates are simple, as all aggregates can be referenced in filter expressions (if you are using a data layer that supports aggregates).

For calculations, only those that define an expression can be referenced in other expressions.

Here are some examples:

```elixir
# given a `full_name` calculation

Ash.Query.filter(User, full_name == "Hob Goblin")

# given a `full_name` calculation that accepts an argument called `delimiter`

Ash.Query.filter(User, full_name(delimiter: "~") == "Hob~Goblin")
```
