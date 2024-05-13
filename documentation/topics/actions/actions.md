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

The resource can have a `default_accept`, declared in its `actions` block, which will be used as the accept list for `create` and `destroy` actions, if they don't define one.

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
