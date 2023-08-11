# Actions

## Action Types

Ash has 5 action types `:read`, `:create`, `:update`, `:destroy` and `:action`. The purpose of these action types is to provide expectations about what is required to run those actions, and what is returned from them.

### Generic Actions

The `:action` type is a special type of action that can do essentially whatever you want. We refer to it as a "generic" action, because there are no special rules about how it works, and minimal structure surrounding it.
A generic action takes arguments and returns a value. The struct used for building input for a generic action is `Ash.ActionInput`. Most of this document we will focus on the four main action types.

### Create/Read/Update/Destroy

The actions do not need to do _exactly_ what their action type implies however. Using manual actions, you can define a create action that actually updates something, or using the `soft?` option for `destroy` actions you can treat them as updates. The important part to consider is their interface. More action types may be added in the future.

Actions either read data or mutate it. `:read` actions are fundamentally different from `:create`, `:update` and `:destroy` actions. For the most part, `:create`, `:update` and `:destroy` follow all of the same rules, and so will be grouped together when explaining how they behave. Small differences will be pointed out in a few places.

## Idiomatic Actions

### Name Your Actions

The intent behind Ash is _not_ to have you building simple CRUD style applications. In a typical set up you may have a resource with four basic actions, there is even a shorthand to accomplish this:

```elixir
actions do
  defaults [:create, :read, :update, :destroy]
end
```

But that is just a simple way to get started, or to create resources that really don't do anything beyond those four operations. You can have _as many actions as you want_. The best designed Ash applications will have numerous actions, named after the intent behind how they are used. They won't have all reads going through a single read action, and the same goes for the other action types. The richer the actions on the resource, the better interface you can have. With that said, many resources may only have those four basic actions, especially those that are "managed" through some parent resource. See the guide on [Managing Relationships](/documentation/topics/managing-relationships.md) for more.

### Primary Actions

Primary actions are a way to inform the framework which actions should be used in certain "automated" circumstances, or in cases where an action has not been specified. If a primary action is attempted to be used but does not exist, you will get an error about it at runtime. The place you typically need primary actions is, when [Managing Relationships](/documentation/topics/managing-relationships.md). However, some prefer to be as explicit as possible, and so will _always_ indicate an action name, and in that case will never use primary actions. When using the `defaults` option to add default actions, they are marked as primary.

A simple example where a primary action would be used:

```elixir
# No action is specified, so we look for a primary read.
Api.get!(Resource, "8ba0ab56-c6e3-4ab0-9c9c-df70e9945281")
```

To mark an action as primary, add the option, i.e

```elixir
read :action_name do
  primary? true
end
```

### Put everything inside the action!

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

defaults [:read, ...]
```

And here is the "right way", where the rules about getting the top tickets have been moved into the resource as a nicely named action, and included in the `code_interface` of that resource. The reality of the situation is that `top_tickets/1` is meant to be obsoleted by your Ash resource! Here is how it _should_ be done.

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

Now, whatever code I had that would have called `top_tickets/1` can now call `Helpdesk.Support.Ticket.top(user.id)`. By doing it this way, you get the primary benefit of getting a nice simple Api to call into, but you _also_ have a way to modify how the action is invoked in any way necessary, by going back to the old way of building the query manually. For example, if I also only want to see top tickets that were opened in the last 10 minutes:

```elixir
Ticket
|> Ash.Query.for_read(:top, %{user_id: user.id})
|> Ash.Query.filter(opened_at > ago(10, :minute))
|> Helpdesk.Support.read!()
```

That is the best of both worlds! These same lessons transfer to changeset based actions as well.

## Action Lifecycle

Ash uses an "engine" internally that takes lists of "requests" that have dependencies on each-other, and resolves them in some acceptable order. This engine allows for things like parallelizing steps and performing complex workflows without having to handwrite all of the control flow. It isn't important that you know how the engine works, but knowing the basic idea of "list of requests get sent to the engine" should help contextualize the following flow charts.

### Read Actions

Read actions operate on an `Ash.Query`. They take no input by default, but arguments can be added to the action. All read actions expect to work on lists. The act of pagination, or returning a single result, is handled as part of the interface, and is not a concern of the action itself. Here is an example of a read action:

```elixir
# Giving your actions informative names is always a good idea
read :ticket_queue do
  # Use arguments to take in values to run your read action.
  argument :priorities, {:array, :atom} do
    constraints items: [one_of: [:low, :medium, :high]]
  end

  # This action may be paginated, and returns a total count of records by default
  pagination offset: true, countable: :by_default

  # Use arguments to modify filters
  # You can also use arguments in custom preparations using `Ash.Changeset.get_argument/2`
  # This is useful when a simple filter like the one below does not suffice
  filter expr(status == :open and priority in ^arg(:priorities))
end
```

#### Ash.Query.for_read/4

The following steps are performed when you call `Ash.Query.for_read/4`.

- [Gather Process Context](/documentation/topics/store-context-in-process.md)
- Cast input arguments - `d:Ash.Resource.Dsl.actions.read.argument`
- Set default argument values - `d:Ash.Resource.Dsl.actions.read.argument|default`
- Add errors for missing required arguments | `d:Ash.Resource.Dsl.actions.read.argument|allow_nil?`
- Run query preparations | `d:Ash.Resource.Dsl.actions.read.prepare`
- Add action filter | `d:Ash.Resource.Dsl.actions.read|filter`

#### Running the Read Action

If the query has not yet been run through `Ash.Query.for_read/3` for the action in question, we do that first. Then we perform the following steps. These steps are trimmed down, and are aimed at helping users understand the general flow. Some steps are omitted.

- Run `Ash.Query.for_read/3` if it has not already been run
- [Apply tenant filters for attribute](/documentation/topics/multitenancy.md)
- Apply [pagination](/documentation/topics/pagination.md) options
- Run before action hooks
- Multi-datalayer filter is synthesized. We run queries in other data layers to fetch ids and translate related filters to `(destination_field in ^ids)`
- Strict Check & Filter Authorization is run
- Data layer query is built and validated
- Data layer query is Run
- Authorizer "runtime" checks are run (you likely do not have any of these)

The following steps happen asynchronously during or after the main data layer query has been run

- If paginating and count was requested, the count is determined at the same time as the query is run.
- Any calculations & aggregates that were able to be run outside of the main query are run
- Any relationships are loaded

### Create/Update/Destroy Actions

These actions operate on an `Ash.Changeset`. While standard destroy actions don't care about the changes you add to a changeset, you may mark a destroy action as `d:Ash.Resource.Dsl.actions.destroy|soft?`, which means you will be performing an update that will in some way "hide" the resource. Generally this hiding is done by adding a `d:Ash.Resource.Dsl.resource|base_filter` i.e `base_filter [is_nil: :archived_at]`

Here is an example create action:

```elixir
create :register do
  # By default all public attributes are accepted, but this should only take email
  accept [:email]

  # Accept additional input by adding arguments
  argument :password, :string do
    allow_nil? false
  end

  argument :password_confirmation, :string do
    allow_nil? false
  end

  # Use the built in `confirm/2` validation
  validate confirm(:password, :password_confirmation)

  # Call a custom change that will hash the password
  change MyApp.User.Changes.HashPassword
end
```

#### Changesets for actions

The following steps are run when calling `Ash.Changeset.for_create/4`, `Ash.Changeset.for_update/4` or `Ash.Changeset.for_destroy/4`.

- [Gather process context](/documentation/topics/store-context-in-process.md)
- Cast input params | This is any arguments in addition to any accepted attributes
- Set argument defaults
- Require any missing arguments
- Validate all provided attributes are accepted
- Require any accepted attributes that are `allow_nil?` false
- Set any default values for attributes
- Run action changes & validations
- Run validations, or add them in `before_action` hooks if using `d:Ash.Resource.Dsl.actions.create.validate|before_action?`. Any global validations are skipped if the action has `skip_global_validations?` set to `true`.

#### Running the Create/Update/Destroy Action

All of these actions are run in a transaction if the data layer supports it. You can opt out of this behavior by supplying `transaction?: false` when creating the action. When an action is being run in a transaction, all steps inside of it are serialized, because generally speaking, transactions cannot be split across processes.

- Authorization is performed on the changes
- A before action hook is added to set up belongs_to relationships that are managed. This means potentially creating/modifying the destination of the relationship, and then changing the `destination_attribute` of the relationship.
- Before transaction hooks are called (`Ash.Changeset.before_transaction/2`). Keep in mind, any validations that are marked as `before_action? true` (or all global validations if your action has `delay_global_validations? true`) will not have happened at this point.
- A transaction is opened if the action is configured for it (by default they are) and the data layer supports transactions
- Before action hooks are performed in reverse order they were added. (unless `append?` option was used)
- For manual actions, a before action hook must have set
- After action hooks are performed in the order they were added (unless `prepend?` option was used)
- For [Manual Actions](/documentation/topics/manual-actions.md), one of these after action hooks must have returned a result, otherwise an error is returned.
- Non-belongs-to relationships are managed, creating/updating/destroying related records.
- A transaction is opened if the action is configured for it (by default they are) and the data layer supports transactions
- If an `after_action` option was passed when running the action, it is run with the changeset and the result. Only supported for create & update actions.
- The transaction is closed, if one was opened
- After transaction hooks are invoked with the result of the transaction (even if it was an error)

## Generic Actions

A generic action consists of three main components:

1. the return type
2. the arguments
3. the `run` function

Here is an example:

```elixir
action :hello, :string do
  argument :name, :string, allow_nil?: false

  run(fn input, _context ->
    {:ok, "Hello #{input.arguments.name}"}
  end)
end
```

The benefit of using generic actions instead of defining normal functions:

- They can be used with api extensions
- They support Ash authorization patterns (i.e policies)
- They be included in the code interface of a resource
- They can be made transactional with a single option (`transaction? true`)
