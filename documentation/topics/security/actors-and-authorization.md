# Actors & Authorization

Authorization in Ash involves three things:

* actor - the entity (i.e user, organization, device) performing a given action
* authorize? - a flag that tells Ash to run authorization.
* authorizers - the extensions on a resource that can modify or forbid the action.

## Setting `actor` and `authorize?`

All functions in Ash that may perform authorization and/or wish to use the actor accept an `actor` and an `authorize?` option. For example:

Building a changeset/query/input is the best time to provide the actor option

```elixir
Ash.Changeset.for_create(Post, %{title: "Post Title"}, actor: current_user, authorize?: true)
```

If calling a function without changeset/query/input, you can provide the `actor` option at that point.

```elixir
Ash.count!(Post, actor: current_user, authorize?: true)
```

Functions created with the code interface also accept an `actor` option.

```elixir
MyDomain.create_post!(Post, authorize?: true)
```

> ### Set the actor on the query/changeset/input {: .warning}
> The hooks on a query/changeset/input to an action may need to know the actor, so you
> need to set the actor when building them, not when calling the action.
> ```elixir
> # DO THIS
>
> Post
> |> Ash.Query.for_read(:read, %{}, actor: current_user)
> |> Ash.read!()
>
> # DON'T DO THIS
>
> Post
> |> Ash.Query.for_read!(:read)
> |> Ash.read!(actor: current_user)
> ```

## Default value of `authorize?`

The default value of `authorize?` is determined by the `authorization` configuration of the relevant domain. By default, `authorize?` is set to `true` (and so can be ommitted in all of the examples above). If a resource has no authorizers, then all requests will be allowed.

## Authorizers

Authorizers are in control of what happens during authorization. Generally, you won't need to create your own authorizer, as the builtin policy authorizer `Ash.Policy.Authorizer` works well for any use case. See the [Policies guide](documentation/topics/security/policies.md) for more.

## Domain Authorization Configuration

### `d:Ash.Domain.Dsl.authorization|require_actor?`

Requires that an actor is set for all requests.

Important: `nil` is still a valid actor, so this won't prevent providing `actor: nil`. It only requires that the option itself is provided.

### `d:Ash.Domain.Dsl.authorization|authorize`

When to run authorization for a given request.

- `:by_default` sets `authorize?: true` if the `authorize?` option was not set (so it can be set to `false`). This is the default.
- `:always` forces `authorize?: true` on all requests to the domain.
- `:when_requested` sets `authorize?: true` whenever an actor is set or `authorize?: true` is explicitly passed.
