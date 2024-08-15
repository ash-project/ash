# Code Interface

One of the ways that we interact with our resources is via hand-written code. The general pattern for that looks like building a query or a changeset for a given action, and calling it via functions like `Ash.read/2` and `Ash.create/2`. This, however, is just one way to use Ash, and is designed to help you build tools that work with resources, and to power things like `AshPhoenix.Form`, `AshGraphql.Resource` and `AshJsonApi.Resource`. When working with your resources in code, we generally want something more idiomatic and simple. For example, on a domain called `Helpdesk.Support`.

```elixir
resources do
  resource Ticket do
    define :open_ticket, args: [:subject], action: :open
  end
end
```

This simple setup now allows you to open a ticket with `Helpdesk.Support.open_ticket(subject)`. You can cause it to raise errors instead of return them with `Helpdesk.Support.open_ticket!(subject)`. For information on the options and additional inputs these defined functions take, look at the generated function documentation, which you can do in iex with `h Helpdesk.Support.open_ticket`. For more information on the code interface, read the DSL documentation: `d:Ash.Domain.Dsl.resource.interfaces`.

## Code interfaces on the resource

You can define a code interface on individual resources as well, using the `code_interface` block. The DSL is the same as the DSL for defining it in the `domain`. For example:

```elixir
code_interface do
  # the action open can be omitted because it matches the functon name
  define :open, args: [:subject]
end
```

These will then be called on the resource itself, i.e `Helpdesk.Support.Ticket.open(subject)`.

## Using the code interface

If the action is an update or destroy, it will take a record or a changeset as its _first_ argument.
If the action is a read action, it will take a starting query as an _opt in the last_ argument.

All functions will have an optional last argument that accepts options. See `Ash.Resource.Interface.interface_options/2` for valid options.

For reads:

- `:query` - a query to start the action with, can be used to filter/sort the results of the action.

For creates:

- `:changeset` - a changeset to start the action with

They will also have an optional second to last argument that is a freeform map to provide action input. It _must be a map_.
If it is a keyword list, it will be assumed that it is actually `options` (for convenience).
This allows for the following behaviour:

```elixir
# Because the 3rd argument is a keyword list, we use it as options
Accounts.register_user(username, password, [tenant: "organization_22"])
# Because the 3rd argument is a map, we use it as action input
Accounts.register_user(username, password, %{key: "val"})
# When all arguments are provided it is unambiguous
Accounts.register_user(username, password, %{key: "val"}, [tenant: "organization_22"])
```

## Calculations

Resource calculations can be run dynamically using `Ash.calculate/3`, but
you can also expose them using the code_interface with `define_calculation`.

For example:

```elixir
calculations do
  calculate :full_name, :string, expr(first_name <> ^arg(:separator) <> last_name) do
    argument :separator, :string do
      allow_nil? false
      default " "
    end
  end
end

# in your domain
resource User do
  define_calculation :full_name, args: [:first_name, :last_name, {:optional, :separator}]
  # or if you want to take a record as an argument
  define_calculation :full_name, args: [:_record]
end
```

This could now be used like so:

```elixir
Accounts.full_name("Jessie", "James", "-")
# or with a record as an argument
Accounts.full_name(user)
```

This allows for running calculations without an instance of a resource, normally done via `Ash.load(user, :full_name)`

By default, configured args will be provided for any matching named reference _or_ argument. This is normally fine, but in the case that you have an argument and a reference with the same name, you can specify it by supplying `{:arg, :name}` and `{:ref, :name}`. For example:

```elixir
define_calculation :id_matches, args: [{:arg, :id}, {:ref, :id}]
```

To make arguments optional, wrap them in `{:optional, ..}`, for example:

```elixir
define_calculation :id_matches, args: [{:arg, :id}, {:optional, {:ref, :id}}]
```

## Bulk & atomic actions

### Bulk Updates & Destroys

Updates support a list, stream, or query as the first argument. This allows for bulk updates. In this mode, an `%Ash.BulkResult{}` is returned.

> ### Valid inputs {: .warning}
>
> You cannot provide "any enumerable", only lists, streams (a function or a %Stream{}), and queries. We have to be able to distinguish the input as a bulk input and not input to the action itself.

For example:

```elixir
Post
|> Ash.Query.filter(author_id == ^author_id)
|> MyApp.Blog.archive_post!()
# => %Ash.BulkResult{}

[%Post{}, %Post{}]
|> MyApp.Blog.destroy_post!()
# => %Ash.BulkResult{}
end
```

You can pass options to the bulk operation with the `bulk_options` option to your code interface function.

### Bulk Creates

For bulk creates, you can provide a list or stream of inputs. In this mode also, an `%Ash.BulkResult{}` is returned.

> ### Valid inputs {: .warning}
>
> You cannot provide "any enumerable", only lists, streams (a function or a %Stream{}). We have to be able to distinguish the input as a bulk input and not input to the action itself.

Any arguments on the code interface will be applied to _all_ inputs given as a list, and the arguments will come first.

```elixir
[%{title: "Post 1"}, %{title: "Post 2"}, ...]
# if `:special` is an action argument, it will be applied to all inputs
|> MyApp.Blog.create_post!(:special, bulk_options: [batch_size: 10])
```

### Returning streams from read actions

The `:stream?` option allows you to return a stream to be enumerated later.

For example:

```elixir
MyApp.Blog.my_posts(stream?: true, actor: me)
# => #Stream<...>
```
