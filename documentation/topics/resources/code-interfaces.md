<!--
SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs.contributors>

SPDX-License-Identifier: MIT
-->

# Code Interface

One of the ways that we interact with our resources is via hand-written code. The general pattern for that looks like building a query or a changeset for a given action, and calling it via functions like `Ash.read/2` and `Ash.create/2`. This, however, is just one way to use Ash, and is designed to help you build tools that work with resources, and to power things like `AshPhoenix.Form`, `AshGraphql.Resource` and `AshJsonApi.Resource`. When working with your resources in code, we generally want something more idiomatic and simple. For example, on a domain called `Helpdesk.Support`.

```elixir
resources do
  resource Ticket do
    define :open_ticket, args: [:subject], action: :open
  end
end
```

This simple setup now allows you to open a ticket with `Helpdesk.Support.open_ticket(subject)`. You can cause it to raise errors instead of return them with `Helpdesk.Support.open_ticket!(subject)`. For information on the options and additional inputs these defined functions take, look at the generated function documentation, which you can do in iex with `h Helpdesk.Support.open_ticket`. For more information on the code interface, read the DSL documentation: `d:Ash.Domain.Dsl.resources.resource.define`.

## Code interfaces on the resource

You can define a code interface on individual resources as well, using the `code_interface` block. The DSL is the same as the DSL for defining it in the `domain`. For example:

```elixir
code_interface do
  # the action open can be omitted because it matches the function name
  define :open, args: [:subject]
end
```

These will then be called on the resource itself, i.e `Helpdesk.Support.Ticket.open(subject)`.

## Using the code interface

If the action is an update or destroy, it will take a record or a changeset as its _first_ argument.
If the action is a read action, it will take a starting query as an _opt in the last_ argument.

All functions will have an optional last argument that accepts options. See `Ash.Resource.Interface` for valid options.

For reads:

- `:query` - a query to start the action with, can be used to filter/sort the results of the action. This can be a keyword list of any of the options that `Ash.Query.build/3` supports.

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

For a full list of options, see the functions in `Ash.Resource.Interface`, or use iex help on your generated
functions, i.e

```elixir
iex> h Accounts.register_user/3
```

### get_by functions

A common pattern in Ash applications is the "get by" function for retrieving individual records. This pattern provides a clean alternative to using `Ash.get!/2` directly in your web modules.

**Avoid this pattern:**

```elixir
# In a LiveView or Controller - DON'T DO THIS
group =
  MyApp.Ash.Dashboards.DashboardGroup
  |> Ash.get!(group_id)
  |> Ash.load!(students: [:user])
```

This is similar to using `Repo.get/2` and `Repo.preload/2` directly outside of context modules, which is generally considered a bad practice.

**Use this pattern instead:**

```elixir
# In your domain
resource DashboardGroup do
  define :get_dashboard_group_by_id, action: :read, get_by: [:id]
end

# In your LiveView or Controller
group = MyApp.Dashboards.get_dashboard_group_by_id!(group_id)
```

The `get_by` option automatically creates a function that:
- Uses the primary read action with an applied filter
- Supports dynamic loading and filtering through the standard options
- Provides both raising (`!`) and non-raising versions

**Dynamic loading and filtering:**

Code interfaces automatically support loading and filtering options:

```elixir
# Load relationships
MyApp.Dashboards.get_dashboard_group_by_id!(id, load: [students: [:user]])

# Apply additional filters
MyApp.Dashboards.get_dashboard_group_by_id!(id, query: [filter: [status: :active]])

# Combine both
MyApp.Dashboards.get_dashboard_group_by_id!(id,
  load: [students: [:user]],
  query: [filter: [status: :active]]
)
```

The `query` option accepts either an `Ash.Query` struct or a keyword list that gets passed to `Ash.Query.build/2`.

**When to use actions vs code interfaces:**

- **Actions** define what operations are possible on your resource
- **Code interfaces** provide function-based access to those actions
- Actions don't require code interfaces and can be used by extensions like `AshJsonApi`
- Code interfaces make actions callable as functions (e.g., `DashboardGroup.get_by_id/1`)

This pattern encourages proper separation of concerns by keeping domain logic in your resources and providing clean interfaces for your web layer.

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

### Customizing the generated function

Often we want to have a slightly different interface when calling actions with functions,
or we want to maintain backwards compatibility for callers of our code interface while
changing the underlying action implementation.

You can define `custom_input`s on your code interfaces to massage arguments from the function
into a shape expected by the action.

For example, lets say we have an action that accepts an `artist_id` as an argument. We want
it to use `artist_id` for two reasons: it is the only part of the artist required to perform
the action, and accepting ids is better for an action supporting usage over an API. However,
we want the function itself to accept either an artist *or* an artist_id.

```elixir
define :follow_artist do
  action :follow

  # `artist` (from the custom input below) is a positional argument to the function
  args [:artist]

  # make a custom input called `artist`, that is a union type
  custom_input :artist, :union do
    # allow passing either an artist or an artist_id
    constraints types: [
      artist: [type: :struct, constraints: [instance_of: Artist]],
      artist_id: [type: :uuid]
    ]

    transform do
      # Pass it to the action as `artist_id`
      to :artist_id

      # Extracting the value using this function
      using fn
        %Ash.Union{type: :artist, value: value} ->
          value.id
        %Ash.Union{type: :artist_id, value: value} ->
          value
      end
    end
  end
end
```

The example above is a bit verbose. In practice we might create a type, called `ArtistOrId`,
for example, and extract that logic like so:

```elixir
defmodule MyApp.Types.ArtistOrId do
  use Ash.Type.NewType, subtype_of: :union, constraints: [
    types: [
      artist: [type: :struct, constraints: [instance_of: Artist]],
      artist_id: [type: :uuid]
    ]
  ]

  def to_artist_id(%Ash.Union{type: :artist, value: artist}), do: artist.id
  def to_artist_id(%Ash.Union{type: :artist_id, value: artist_id}), do: artist_id
end
```

And then we can refactor the above example like so:

```elixir
define :follow_artist do
  action :follow
  args [:artist]

  custom_input :artist, MyApp.Types.ArtistOrId do
    transform do
      to :artist_id
      using &MyApp.Types.ArtistOrId.to_artist_id/1
    end
  end
end
```

### Default Options

You can provide default options that will be merged with client-provided options using the `default_options` configuration. This is useful for setting common options that should be applied to all calls of a particular interface function.

`default_options` accepts either:
- A keyword list of static options
- A zero-arity function that returns a keyword list of dynamic options

#### Static Default Options

```elixir
code_interface do
  define :get_user, action: :read, get_by: [:id],
    default_options: [
      load: [:profile, :posts]
    ]
end
```

With this configuration, calling `MyApp.get_user(123)` will automatically load the profile and posts relationships and enable authorization.

#### Dynamic Default Options

For options that need to be computed at runtime, you can provide a function:

```elixir
code_interface do
  define :get_user_with_timestamp, action: :read, get_by: [:id],
    default_options: fn ->
      [
        load: [:profile],
        context: %{
          requested_at: DateTime.utc_now(),
          request_id: System.unique_integer()
        }
      ]
    end
end
```

The function is called each time the interface function is invoked, allowing for dynamic values like timestamps or request IDs.

#### Option Merging Behavior

Default options are merged with client-provided options using the following rules:

- Most options: Client options override defaults
- `:load` options: Combined (both default and client loads are applied)
- `:bulk_options` and `:page` options: Deep merged (keyword lists are merged together)

Example:
```elixir
# With default_options: [load: [:profile], authorize?: true]
MyApp.get_user(123, load: [:posts], authorize?: false)
# Results in: [load: [:profile, :posts], authorize?: false]
```

### Authorization Functions

For each action defined in a code interface, Ash automatically generates corresponding authorization check functions:

- `can_action_name?(actor, params \\ %{}, opts \\ [])` - Returns `true`/`false` for authorization checks
- `can_action_name(actor, params \\ %{}, opts \\ [])` - Returns `{:ok, true/false}` or `{:error, reason}`

Example usage:
```elixir
# Check if user can create a post
if MyApp.Blog.can_create_post?(current_user) do
  # Show create button
end

# Check if user can update a specific post
if MyApp.Blog.can_update_post?(current_user, post) do
  # Show edit button
end

# Check if user can destroy a specific comment
if MyApp.Blog.can_destroy_comment?(current_user, comment) do
  # Show delete button
end
```

These functions are particularly useful for conditional rendering of UI elements based on user permissions.

#### Authorization Function Options

The authorization functions accept the same options as `Ash.can/3` and `Ash.can?/3`, including:

- `log?` - Whether to log the authorization result at `:info` level for debugging purposes

```elixir
# Enable logging for this specific authorization check
if MyApp.Blog.can_update_post?(current_user, post, log?: true) do
  # Show edit button
end

# This will log authorization details to help with debugging
MyApp.Blog.can_create_post(current_user, %{title: "New Post"}, log?: true)
```
