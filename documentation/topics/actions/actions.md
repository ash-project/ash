# Actions

In Ash, actions are the primary way to interact with your resources. There are five types of actions:

- [Read](documentation/topics/actions/read-actions.md)
- [Create](documentation/topics/actions/create-actions.md)
- [Update](documentation/topics/actions/update-actions.md)
- [Destroy](documentation/topics/actions/destroy-actions.md)
- [Generic](documentation/topics/actions/generic-actions.md)

All actions can be run in a transaction. Create, update and destroy actions are run in a transaction by _default_, whereas read and generic actions require opting in with `transaction? true` in the action definition. Each action has its own set of options, ways of calling it, and ways of customizing it. See the relevant guide for specifics on each action type. This topic focuses on idiomatic ways to use actions, and concepts that cross all action types.

## Primary Actions

Primary actions are a way to inform the framework which actions should be used in certain "automated" circumstances, or in cases where an action has not been specified. If a primary action is attempted to be used but does not exist, you will get an error about it at runtime.

The place you typically need primary actions is when [Managing Relationships](/documentation/topics/resources/relationships.md#managing-relationships). When using the `defaults` option to add default actions, they are marked as primary.

A simple example where a primary action would be used:

```elixir
# No action is specified, so we look for a primary read.
Ash.get!(Resource, "8ba0ab56-c6e3-4ab0-9c9c-df70e9945281")
```

To mark an action as primary, add the option, i.e

```elixir
read :action_name do
  primary? true
end
```

## Accepting Inputs

[Create](documentation/topics/actions/create-actions.md) and [Update](documentation/topics/actions/update-actions.md) actions can accept attributes as input. There are two primary ways that you annotate this.

### Using `accept` in specific actions

Each action can define what it accepts, for example:

```elixir
create :create do
  accept [:name, :description]
end
```

You could then pass in `%{name: "a name", description: "a description"}` to this action.

### Using `default_accept` for all actions

The resource can have a `default_accept`, declared in its `actions` block, which will be used as the accept list for `create` and `update` actions, if they don't define one.

```elixir
actions do
  default_accept [:name, :description]

  create :create
  update :update

  update :special_update do
    accept [:something_else]
  end
end
```

In the example above, you can provide `%{name: "a name", description: "a description"}` to both the `:create` and `:update` actions, but only `%{something_else: "some_value"}` to `:special_update`.

#### Using module attributes for action specific accept lists

You can also use module attributes to define the accept list.  This is useful if you have a lot of attributes and different variations for different actions.

```elixir
@accepts_special_update [:name, :description, :foo, :bar, :baz]

@accepts_super_special_update @accepts_special_update ++ [:something_else, :another_thing]

actions do
  default_accept [:name, :description]

  create :create
  update :update

  update :special_update do
    accept @accepts_special_update
  end
end
```

This is extremely simple example

## Context

There are two kinds of contexts in Ash:

1. the context given to a changeset/action call, stored in `changeset.context`,
2. the context given to a callback function like `c:Ash.Resource.Change.change/3`, which contains
  the above context in it's `source_context` key, as well as additional information specific to the callback,
  and/or commonly needed keys for callbacks (actor, tenant, etc.).

Actions accept a free-form map of context, which can be used for whatever you like. Whenever context is set, it is *deep merged*. I.e if you do `changeset |> Ash.Changeset.set_context(%{a: %{b: 1}}) |> Ash.Changeset.set_context(%{a: %{c: 2}})`, the resulting context will be `%{a: %{b: 1, c: 2}}`. Structs are not merged.

There are some special keys in context to note:

### `:private`

The `:private` key is reserved for use by `Ash` itself. You shouldn't read from or write to it.

### `:shared`

The `:shared` key will be passed to all nested actions built by Ash, and should be passed by you to any actions you call within changes/preparations etc. Whenever `:shared` context
is set, it is also written to the outer context. For example `set_context(%{shared: %{locale: "en"}})` is equivalent to `set_context(%{shared: %{locale: "en"}, locale: "en"})`

This will generally happen automatically if you use one of the two abstractions provided by Ash for threading options through to nested action calls.

> ### Careful with shared {: .warning}
>
> Shared context is passed to all nested actions, so don't pass massive values around, and also don't set context

## `:query_for`

This is set on queries when they are being run for a "special" purpose. The values this can take are:

- none, if a read action is being run, then no value is set for this context
- `:bulk_update`, if the query is being built to power a bulk update action
- `:bulk_destroy`, if the query is being built to power a bulk destroy action
- `:load`, if the query is being built to power an `Ash.load` call

You can use this to adjust the behavior of your query preparations as needed.

## `:bulk_create`, `:bulk_update`, `:bulk_destroy`

This is set on changesets when they are being run in bulk. The value will be a map with the following keys (more may be added in the future):

`:index` -> The index of the changeset in the bulk operation.

#### `Ash.Scope.ToOpts`

`Ash.Scope.ToOpts` is newer and is the recommended way to do this. In action callbacks in Ash, you will be provided with a context, which can be passed down as a `scope` option when running nested actions or building nested changesets/queries. For example:

```elixir
def change(changeset, opts, context) do
  Ash.Changeset.after_action(changeset, fn changeset, result ->
    # automatically passes the `shared` context to the nested action
    MyApp.MyDomain.create_something_else(..., scope: context, other: :options)
  end)
end
```

To get the opts for a given scope, you can use `Ash.Scope.to_opts(scope)`, but this is typically not
necessary.

#### `Ash.Context.to_opts/2`

`Ash.Context.to_opts/2` is a helper function that converts a context map into a list of options that can be passed to nested actions. It automatically passes the `shared` context to the nested action as well.

```elixir
def change(changeset, opts, context) do
  Ash.Changeset.after_action(changeset, fn changeset, result ->
    # automatically passes the `shared` context to the nested action
    MyApp.MyDomain.create_something_else(..., Ash.Context.to_opts(context, other: :options))
  end)
end
```

## Idiomatic Actions

### Name Your Actions

The intent behind Ash is _not_ to have you building simple CRUD style applications. In a typical set up you may have a resource with four basic actions, there is even a shorthand to accomplish this:

```elixir
actions do
  defaults [:read, :destroy, create: :*, update: :*]
end
```

But that is just a simple way to get started, or to create resources that really don't do anything beyond those four operations. You can have _as many actions as you want_. The best designed Ash applications will have numerous actions, named after the intent behind how they are used. They won't have all reads going through a single read action, and the same goes for the other action types. The richer the actions on the resource, the better interface you can have. With that said, many resources may only have those four basic actions, especially those that are "managed" through some parent resource. See the guide on [Managing Relationships](/documentation/topics/resources/relationships.md#managing-relationships) for more.

### Put everything inside the action

Ash provides utilities to modify queries and changesets _outside_ of the actions on the resources. This is a very important tool in our tool belt, _but_ it is very easy to abuse. The intent is that as much behavior as possible is put into the action. Here is the "wrong way" to do it. There is a lot going on here, so don't hesitate to check out other relevant guides if you see something you don't understand.

```elixir
def top_tickets(user_id) do
  Ticket
  |> Ash.Query.for_read(:read)
  |> Ash.Query.filter(priority in [:medium, :high])
  |> Ash.Query.filter(representative_id == ^user_id)
  |> Ash.Query.filter(status == :open)
  |> Ash.Query.sort(opened_at: :desc)
  |> Ash.Query.limit(10)
  |> Helpdesk.Support.read!()
end

# in the resource

actions do
  defaults [:read, ...]
end
```

And here is the "right way", where the rules about getting the top tickets have been moved into the resource as a nicely named action, and included in the `code_interface` of that resource. The reality of the situation is that `top_tickets/1` is meant to be obsoleted by your Ash resource! Here is how it _should_ be done.

```elixir
# in the resource

code_interface do
  define :top, args: [:user_id]
end

actions do
  read :top do
    argument :user_id, :uuid do
      allow_nil? false
    end

    prepare build(limit: 10, sort: [opened_at: :desc])

    filter expr(priority in [:medium, :high] and representative_id == ^arg(:user_id) and status == :open)
  end
end
```

Now, whatever code I had that would have called `top_tickets/1` can now call `Helpdesk.Support.Ticket.top(user.id)`. By doing it this way, you get the primary benefit of getting a nice simple interface to call into, but you _also_ have a way to modify how the action is invoked in any way necessary, by going back to the old way of building the query manually. For example, if I also only want to see top tickets that were opened in the last 10 minutes:

```elixir
Ticket
|> Ash.Query.for_read(:top, %{user_id: user.id})
|> Ash.Query.filter(opened_at > ago(10, :minute))
|> Helpdesk.Support.read!()
```

That is the best of both worlds! These same lessons transfer to changeset based actions as well.

## Private Inputs

The concept of a "private input" can be somewhat paradoxical, but it can be used by actions that require something provided by the "system",
as well as something provided by the caller. For example, you may want an `ip_address` input that can't be set by the user. For this,
you have two options.

### Private Options

```elixir
create :create do
  argument :ip_address, :string, allow_nil?: false, public?: false

  ...
end
```

```elixir
Ash.Changeset.for_create(Resource, :create, %{}, private_arguments: %{ip_address: "<ip_address>"})
```

### Context

You can also provide things to the action via `context`. Context is a map that is a free form map provided to the action.
Context is occasionally used by callers to provide additional information that the action may or may not use.

Context is _deep merged_ with any existing context, and also contains a `private` key that is reserved for use by Ash internals.
You should not remove or manipulate the `private` context key in any way.

```elixir
create :create do
  ...
  change fn changeset, _ ->
    changeset.context # %{ip_address: "<ip_address>"}
  end
end
```

```elixir
Ash.Changeset.for_create(Resource, :create, %{}, context: %{ip_address: "<ip_address>"})
```
