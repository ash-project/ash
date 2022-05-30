# Policies

Policies determine what actions on a resource are permitted for a given actor.

You can specify an actor using the code api via the `actor` option, like so:

```elixir
MyApp.MyApi.read(MyResource, actor: current_user)
```

## Important!

Before we jump into the guide, it is critical to understand that the policy code doesn't actually
_do_ anything in the classic sense. It simply builds up a set of policies that are stored for use later.
The checker that reads those policies and authorizes requests may run all, some of, or none of your checks,
depending on the details of the request being authorized.

## Guide

To see what checks are built-in, see `Ash.Policy.Check.BuiltInChecks`

### The Simplest Policy

Lets start with the simplest policy set:

```elixir
policies do
  policy always() do
    authorize_if always()
  end
end
```

Here, we have a single policy. The first argument to `policy` is the "condition". If the condition is true,
then the policy applies to the request. If a given policy applies, then one of the checks inside the policy must authorize that policy. _Every policy that applies_ to a given request must each be authorized for a request to be authorized.

Within this policy we have a single check, declared with `authorize_if`. Checks logically apply from top to bottom, based on their check type. In this case, we'd read the policy as "this policy always applies, and authorizes always".

There are four check types, all of which do what they sound like they do:

- `authorize_if` - if the check is true, the policy is authorized.
- `authorize_unless` - if the check is false, the policy is authorized.
- `forbid_if` - if the check is true, the policy is forbidden.
- `forbid_unless` - if the check is false, the policy is forbidden.

In each case, if the policy is not authorized or forbidden, the flow moves to the next check.

### A realistic policy

In this example, we use some of the provided built in checks.

```elixir
policies do
  # Anything you can use in a condition, you can use in a check, and vice-versa
  # This policy applies if the actor is a super_user
  # Additionally, this policy is declared as a `bypass`. That means that this check is allowed to fail without
  # failing the whole request, and that if this check *passes*, the entire request passes.
  bypass actor_attribute_equals(:super_user, true) do
    authorize_if always()
  end

  # This will likely be a common occurrence. Specifically, policies that apply to all read actions
  policy action_type(:read) do
    # unless the actor is an active user, forbid their request
    forbid_unless actor_attribute_equals(:active, true)
    # if the record is marked as public, authorize the request
    authorize_if attribute(:public, true)
    # if the actor is related to the data via that data's `owner` relationship, authorize the request
    authorize_if relates_to_actor_via(:owner)
  end
end
```

### Access Type

The default access type is `:filter`. In most cases this will be all you need. In the example above, if a user made a request for all instances
of the resource, it wouldn't actually return a forbidden error. It simply attaches the appropriate filter to fetch data that the user can see.
If the actor attribute `active` was `false`, then the request _would_ be forbidden (because there is no data for which they can pass this policy). However, if `active` is `true`, the authorizer would attach the following filter to the request:

```elixir
public or owner == actor(:_primary_key)
```

To understand what `actor(:_primary_key)` means, see the Filter Templates section in `Ash.Filter`

To change this behavior, use `access_type :strict`. With `access_type :strict` you will force the request to fail unless a filter was provided to yield the appropriate data. In this case, any filter that is a subset of the authorization filter would work. For example: `[public: true]`, or `[owner: [id: current_user.id]]`.

Additionally, some checks have more expensive components that can't be checked before the request is run. To enable those, use the `access_type :runtime`. This is still relatively experimental, but this will attempt to run as much of your checks in a strict fashion, and attach as many things as filters as possible, before running the expensive portion of the checks (defined on the check as `c:Ash.Policy.Check.check/4`)

### Custom checks

See `Ash.Policy.Check` for more information on writing custom checks, which you will likely need at some point when the built in checks are insufficient

## Policy Breakdowns

## Explanation

Policy breakdowns can be fetched on demand for a given forbidden error (either an `Ash.Error.Forbidden` that contains one ore more `Ash.Error.Forbidden.Policy`
errors, or an `Ash.Error.Forbidden.Policy` error itself), via `Ash.Policy.Forbidden.Error.report/2`.

Here is an example policy breakdown from tests:

```text
Policy Breakdown
A check status of `?` implies that the solver did not need to determine that check.
Some checks may look like they failed when in reality there was simply no need to check them.
Look for policies with `✘` and `✓` in check statuses.

A check with a `⬇` means that it didn't determine if the policy was authorized or forbidden, and so moved on to the next check.
`🌟` and `⛔` mean that the check was responsible for producing an authorized or forbidden (respectively) status.

If no check results in a status (they all have `⬇`) then the policy is assumed to have failed. In some cases, however, the policy
may have just been ignored, as described above.

  Admins and managers can create posts | ⛔:
    authorize if: actor.admin == true | ✘ | ⬇    
    authorize if: actor.manager == true | ✘ | ⬇
```

To remove the help text, you can pass the `help_text?: false` option, which would leave you with:

```text
Policy Breakdown
  Admins and managers can create posts | ⛔:
    authorize if: actor.admin == true | ✘ | ⬇    
    authorize if: actor.manager == true | ✘ | ⬇
```

## Including in error message

### **IMPORTANT WARNING**

The following configuration should only ever be used in development mode!

### Instructions

For security reasons, authorization errors don't include any extra information, aside from `forbidden`. To have authorization errors include a policy breakdown (without help text)
use the following config.

```elixir
config :ash, :policies, show_policy_breakdowns?: true
```

## Logging

It is generally safe to log authorization error details, even in production. This can be very helpful when investigating certain classes of issue.

To have ash automatically log each authorization failure, use

```elixir
config :ash, :policies, log_policy_breakdowns: :error # Use whatever log level you'd like to use here
```
