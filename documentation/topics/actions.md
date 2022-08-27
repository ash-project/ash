# Actions

## Action Types

There are four action types currently `:read`, `:create`, `:update`, `:destroy`. The purpose of these action types is to provide expectations about what is required to run those actions, and what is returned from them. The actions do not need to do *exactly* what their action type implies. Using manual actions, you will find that you can define a create action that actually updates something, or using the `soft?` option for `destroy` actions you can treat them as updates. The important part is their interface. More action types may be added in the future.

`:read` actions are fundamentally different from `:create`, `:update` and `:destroy` actions. For the most part, `:create`, `:update` and `:destroy` follow all of the same rules, and so will be grouped together when explaining how they behave. Small differences will be pointed out in a few places.

## Idiomatic Actions

### Name Your Actions

The intent behind Ash is *not* to have you building simple crud style applications. In a typical set up you may have a resource with four basic actions, there is even a shorthand to accomplish this:

```elixir
actions do
  defaults [:create, :read, :update, :destroy]
end
```

But that is just a simple way to get started, or to create resources that really don't do anything beyond those four operations. You can have *as many actions as you want*. The best designed Ash applications will have numerous actions, named after the intent behind how they are used. They won't have all reads going through a single read action, and the same goes for the other action types. The richer the actions on the resource, the better interface you can have. With that said, many resources may only have those four basic actions, especially those that are "managed" through some parent resource. See the guide on {{link:ash:guide:Managing Relationships}} for more.

### Primary Actions

Primary actions are a way to inform the framework which actions should be used in certain "automated" circumstances, or in cases where an action has not been specified. If a primary action is attempted to be used but does not exist, you will get an error about it at runtime. The place you typically need primary actions is, when {{link:ash:guide:Managing Relationships}}. However, some prefer to be as explicit as possible, and so will *always* indicate an action name, and in that case will never use primary actions. When using the `defaults` option to add default actions, they are marked as primary.

A simple example where a primary action would be used:

```elixir
# No action is specified, so we look for a primary read.
Api.get!(Resource, "8ba0ab56-c6e3-4ab0-9c9c-df70e9945281")
```

To mark an action as primary, simply add the option, i.e

```elixir
read :action_name do
  primary? true
end
```

### Put everything inside the action!

Ash provides utilities to modify queries and changesets *outside* of the actions on the resources. This is a very important tool in our tool belt, *but* it is very easy to abuse. The intent is that as much behavior as possible is put into the action. Here is the "wrong way" to do it. There is a lot going on here, so don't hesitate to check out other relevant guides if you see something you don't understand.

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

defaults [:read, ...]
```

And here is the "right way", where the rules about getting the top tickets have been moved into the resource as a nicely named action, and included in the `code_interface` of that resource. The reality of the situation is that `top_tickets/1` is meant to be obsoleted by your Ash resource! Here is how it *should* be done.

```elixir
# in the resource

code_interface do
  define_for Helpdesk.Support

  define :top, args: [:user_id]
end

read :top do
  argument :user_id, :uuid do
    allow_nil? false
  end

  prepare build(limit: 10, sort: [opened_at: :desc])

  filter expr(priority in [:medium, :high] and representative_id == ^arg(:user_id) and status == :open)
end
```

Now, whatever code I had that would have called `top_tickets/1` can now simply call `Helpdesk.Support.Ticket.top(user.id)`. By doing it this way, you get the primary benefit of getting a nice simple Api to call into, but you *also* have a way to modify how the action is invoked in any way necessary, by going back to the old way of simply building the query. For example, if I also only want to see top tickets that were opened in the last 10 minutes:

```elixir
Ticket
|> Ash.Query.for_read(:top)
|> Ash.Query.filter(opened_at > ago(10, :minute))
|> Helpdesk.Support.read!()
```

That is the best of both worlds! These same lessons transfer to changeset based actions as well.

## How Do Actions Work

### Read Actions

Read actions operate on an `Ash.Query`. They take no input by default, but arguments can be added to the action. All read actions expect to work on lists. The act of pagination, or returning a single result, is handled as part of the interface, and is not a concern of the action itself. Here is an example of a read action:

```elixir
# Giving your actions informative names is always a good idea
# We do
read :ticket_queue do
  argument :priorities, {:array, :atom} do
    constraints items: [one_of: [:low, :medium, :high]]
  end

  filter expr(status == :open and priority in ^arg(:priorities))
end
```

***TBD mermaid chart here, and same type of explanation for create/update/destroy ***
