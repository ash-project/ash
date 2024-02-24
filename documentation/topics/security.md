# Security

## Authorization Configuration

### `d:Ash.Domain.Dsl.authorization|require_actor?`

Requires that an actor is set for all requests.

Important: `nil` is still a valid actor, so this won't prevent providing `actor: nil`.


### `d:Ash.Domain.Dsl.authorization|authorize`

When to run authorization for a given request.

- `:by_default` sets `authorize?: true` if the `authorize?` option was not set (so it can be set to `false`). This is the default.
- `:always` forces `authorize?: true` on all requests to the domain.
- `:when_requested` sets `authorize?: true` whenever an actor is set or `authorize?: true` is explicitly passed. This is the default behavior.


## Sensitive Attributes

Using `sensitive? true` will cause the argument to be `** Redacted **` from the resource when logging or inspecting. In filter statements, any value used in the same expression as a sensitive attribute will also be redacted. For example, you might see: `email == "** Redacted **"` in a filter statement if `email` is marked as sensitive.

## Authorization

Authorization in Ash is done via authorizers. Generally, you won't need to create your own  authorizer, as the builtin policy authorizer `Ash.Policy.Authorizer` should work well for any use case. Authorization is performed with a given actor and a query or changeset.

### Actors

An actor is the "entity performing the action". This is generally a user, but could potentially be an organization, a group, whatever makes sense for your use case. By default, when using Ash in code, authorization does not happen.

```elixir
# Does not perform authorization
Ash.read!(User)
```

However, if you either 1. provide an actor or 2. use the `authorize?: true` option, then authorization will happen.

```elixir
# Authorize with a `nil` actor (which is valid, i.e if no one is logged in and they are trying to list users)
Ash.read!(User, actor: nil)

# Authorize with a `nil` actor
Ash.read!(User, authorize?: true)

# Authorize with an actor
Ash.read!(User, actor: current_user)

# Authorize with an actor, but being explicit
Ash.read!(User, actor: current_user, authorize?: true)

# Skip authorization, but set an actor. The actor can be used in other things than authorization
# so this may make sense depending on what you are doing.
Ash.read!(User, actor: current_user, authorize?: false)
```

#### Where to set the actor

When setting an actor, if you are building a query or changeset, you should do so at the time that you call the various `for_*` functions. This makes the actor available in the context of any change that is run. For example:

```elixir
# DO THIS
Resource
|> Ash.Query.for_read(:read, input, actor: current_user)
|> Ash.read()

# DON'T DO THIS
Resource
|> Ash.Query.for_read(:read, input)
|> Ash.read(actor: current_user)
```

The second option "works" in most cases, but not all, because some `change`s might need to know the actor

### Context

Ash can store the actor, query context, or tenant in the process dictionary. This can help simplify things like live views, controllers, or channels where all actions performed share these pieces of context.

This can be useful, but the general recommendation is to be explicit by passing options.

```elixir
# in socket connect, liveview mount, or a plug
Ash.set_actor(current_user)

# This will now use the actor set in the context.
Ash.read!(User)
```
