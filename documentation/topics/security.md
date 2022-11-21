# Security

## Important Note!

A great thing to do early on is to be explicit about your security configuration. To that end, once you've read this guide, we highly recommend that you place the configuration found at the bottom of your guide into your api modules, even if you are simply setting them to their default values. Especially the `authorize` option.

## Authorization

Authorization in Ash is done via authorizers. Generally, you won't need to create your own  authorizer, as the builtin policy authorizer {{link:ash:extension:Policy Authorizer}} should work well for any use case. Authorization is performed with a given actor and a query or changeset.

### Actors

An actor is the "entity performing the action". This is generally a user, but could potentially be an organization, a group, whatever makes sense for your use case. By default, when using Ash in code, authorization does not happen.

```elixir
# Does not perform authorization
Api.read!(User)
```

However, if you either 1. provide an actor or 2. use the `authorize?: true` option, then authorization will happen.

```elixir
# Authorize with a `nil` actor (which is valid, i.e if no one is logged in and they are trying to list users)
Api.read!(User, actor: nil)

# Authorize with a `nil` actor
Api.read!(User, authorize?: true)

# Authorize with an actor
Api.read!(User, actor: current_user)

# Authorize with an actor, but being explicit
Api.read!(User, actor: current_user, authorize?: true)

# Skip authorization, but set an actor. The actor can be used in other things than authorization
# so this may make sense depending on what you are doing.
Api.read!(User, actor: current_user, authorize?: false)
```

#### Where to set the actor

When setting an actor, if you are building a query or changeset, you should do so at the time that you call the various `for_*` functions. This makes the actor available in the context of any change that is run. For example:

```elixir
# DO THIS
Resource
|> Ash.Query.for_read(:read, input, actor: current_user)
|> Api.read()

# DON'T DO THIS
Resource
|> Ash.Query.for_read(:read, input, actor: current_user)
|> Api.read(actor: current_user)
```

The second option "works" in most cases, but not all, because some `change`s might need to know the actor

### Context

Ash can store the actor, query context, or tenant in the process dictionary. This can help simplify things like live views, controllers, or channels where all actions performed share these pieces of context.

This can be useful, but the general recommendation is to be explicit by passing options.

```elixir
# in socket connect, liveview mount, or a plug
Ash.set_actor(current_user)

# This will now use the actor set in the context.
Api.read!(User)
```

### Authorization Configuration

The default behavior is illustrated above, but it can be customized with the options in the {{link:ash:dsl:api/authorization}} section of the Api module you are calling.

#### {{link:ash:option:api/authorization/require_actor?}}

Requires that an actor is set for all requests.

Important: `nil` is still a valid actor, so this won't prevent providing `actor: nil`.


#### {{link:ash:option:api/authorization/authorize}}

##### Important!

The default value for this is relatively loose, and we intend to change it in the 3.0 release (which is not scheduled for anytime soon). Right now, it is `:when_requested`, but a better default would be `:by_default`, and is what you should choose when starting out. 

When to run authorization for a given request.

- `:always` forces `authorize?: true` on all requests to the Api. 
- `:by_default` sets `authorize?: true` if the `authorize?` option was not set (so it can be set to `false`).
- `:when_requested` sets `authorize?: true` whenever an actor is set or `authorize?: true` is explicitly passed. This is the default behavior.