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

To see what checks are built-in, see `Ash.Policy.Check.Builtins`

### Basics

#### Policy

Every policy that applies must pass for a given request.
For example, a policy might have a condition `action_type(:read)` and another one might
have a condition like `actor_attribute_equals(:admin, true)`. 
If both apply (i.e an admin is using a read action), then both policies must pass.
A policy can produce one of three results: `:forbidden`, `:authorized`, or `:unknown`. `:unknown` is treated the same as a `:forbidden`.
A policy contains checks, which determine whether or not the policy passes for a given request.

#### Bypass

A bypass policy is just like a policy, except if a bypass passes, then other policies after it do _not_ need to pass.
This can be useful for writing complex access rules, or for a simple rule like "an admin can do anything".

#### Check

Checks, like policies, evaluate from top to bottom. A check can produce one of three results, the same that a policy can produce.
While checks are not necessarily evaluated in order, they _logically apply_ in that order, so you may as well think of it in that way. 
It can be thought of as a simple step-through algorithm.

For each check, starting from the top:

- Run the check.
  - If it returns `:authorized`, the policy is `:authorized`
  - If it returns `:forbidden`, the policy is `:forbidden`
  - If it returns `:unknown`, the next check down is checked

#### Filter checks

Most checks won't return a status, but instead return a "filter". Filter checks are applied to the query that is being run, and then the
rest of the checks are run. In general, all checks should be filter checks or simple checks.

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

### Expression Policies

A simple way to define a policy is by using `expr/1` in the policy. For example:

```elixir
authorize_if expr(exists(role, name == "owner"))
```

#### Using exists

In these and in other filter checks, it is advised to use `exists/2` when referring to relationships, because of the way that the policy authorizer may mix & match your policies when building filters. There is a semantic difference in filters between `friends.first_name == "ted" and friends.last_name == "dansen"`. This means that you have a *single* friend with the first_name "bob" and the last name "fred". If you use `exists`, then your policies can be used in filters without excluding unnecessary data, i.e `exists(friends, first_name == "ted") and exists(friends, last_name == "dansen")` means "you have one friend with the first_name "ted" and one friend with the last_name "dansen".

#### How expressions are used

Depending on the action type these expressions behave slightly differently.

- In reads, the expression will be applied to the query.
- For creates, the expression applies to the result of *applying* the changes. In these cases, you can't use things like `fragment` because nothing exists in the database.
- For updates and destroys, the expression applies to the data *about* to be updated or destroyed

### Access Type

The default access type is `:filter`. In most cases this will be all you need. In the example above, if a user made a request for all instances
of the resource, it wouldn't actually return a forbidden error. It simply attaches the appropriate filter to fetch data that the user can see.
If the actor attribute `active` was `false`, then the request _would_ be forbidden (because there is no data for which they can pass this policy). However, if `active` is `true`, the authorizer would attach the following filter to the request:

```elixir
public or owner == ^actor(:_primary_key)
```

To understand what `actor(:_primary_key)` means, see the Filter Templates section in `Ash.Filter`

Additionally, some checks have more expensive components that can't be checked before the request is run. To enable those, use the `access_type :runtime`. All checks that can be implemented as filters or strict checks will still be done that way, but this enables checks to run their `check/4` callback if necessary.

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
