# Policies

Policies determine what actions on a resource are permitted for a given actor.

You can specify an actor using the `actor` option, whenever using the code interface or creating changesets/queries like so:

```elixir
MyApp.MyApi.read(MyResource, actor: current_user)

MyResource 
|> Ash.Query.for_read(:read, %{}, actor: current_user)

MyResource 
|> Ash.Changeset.for_create(:create, %{}, actor: current_user)
```

## Important!

Before we jump into the guide, it is critical to understand that the policy code doesn't actually
_do_ anything in the classic sense. It only builds up a set of policies that are stored for use later.
The checker that reads those policies and authorizes requests may run all, some of, or none of your checks,
depending on the details of the request being authorized.

## Guide

You'll need to add the extension to your resource, like so:

```elixir
use Ash.Resource,
  authorizers: [Ash.Policy.Authorizer]
```

### Policy

Every policy that applies must pass for a given request.
For example, a policy might have a condition `action_type(:read)` and another one might
have a condition like `actor_attribute_equals(:admin, true)`. 
If both apply (i.e an admin is using a read action), then both policies must pass.
A policy can produce one of three results: `:forbidden`, `:authorized`, or `:unknown`. `:unknown` is treated the same as a `:forbidden`.
A policy contains checks, which determine whether or not the policy passes for a given request.

### Bypass

A bypass policy is just like a policy, except if a bypass passes, then other policies after it _do not need to pass_.
This can be useful for writing complex access rules, or for a simple rule like "an admin can do anything".

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

## IMPORTANT! How a decision is reached

_Not every check must pass!_ This is described above, but is very important so another example is provided here. Checks go from top to bottom, and *the first one that reaches a decision* determines the *policy result*. For example:

```elixir
policy action_type(:create) do
  authorize_if IsSuperUser 
  forbid_if Deactivated
  authorize_if IsAdminUser
  forbid_if RegularUserCanCreate
  authorize_if RegularUserAuthorized
end
```

We check those from top to bottom, so the first one of those that returns `:authorized` or `:forbidden` determines the entire outcome. For example:

```elixir
authorize_if IsSuperUser # if this is true

# None of the rest of them matter matter
forbid_if Deactivated 
authorize_if IsAdminUser
forbid_if RegularUserCanCreate
authorize_if RegularUserAuthorized
```


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
    # unless the actor is an active user, forbid
    forbid_unless actor_attribute_equals(:active, true)
    # if the record is marked as public, authorize
    authorize_if attribute(:public, true)
    # if the actor is related to the data via that data's `owner` relationship, authorize
    authorize_if relates_to_actor_via(:owner)
  end
end
```

### Check

Checks, like policies, evaluate from top to bottom. A check can produce one of three results, the same that a policy can produce.
While checks are not necessarily evaluated in order, they _logically apply_ in that order, so you may as well think of it in that way. 
It can be thought of as a simple step-through algorithm.

For each check, starting from the top:

- Run the check.
  - If it returns `:authorized`, the policy is `:authorized`
  - If it returns `:forbidden`, the policy is `:forbidden`
  - If it returns `:unknown`, the next check down is checked

#### Builtin Checks

To see what checks are built-in, see `Ash.Policy.Check.Builtins`

#### Custom Checks

There are three types of checks. `:simple`, `:filter` and `:manual`. Generally speaking, you will almost always want to write either `:simple` or `:filter` checks. They are both a subset of a `:manual` checks. To implement a manual check, create a module that adopts the `Ash.Policy.Check` behaviour. Simple and Filter checks are documented below.

##### Simple Checks

Simple checks are determined at the outset of a request, and can only cause a request to be authorized or unauthorized. See Filter Checks below for more information on writing checks that can be applied as filters.

```elixir
defmodule MyApp.Checks.ActorIsOldEnough do
  use Ash.Policy.SimpleCheck

  def describe(_) do
    "actor is old enough"
  end

  # The context here has the changeset, query, resource, and api.
  # match? just needs to return true or false, i.e "is the actor old enough"
  def match?(%MyApp.User{age: age}, %{resource: MyApp.Beer}, _) do
    age >= 21
  end

  def match?(_, _, _), do: true
end
```

##### Filter Checks

Many checks won't return a status, but instead return a "filter".
Filter checks can be used in policies that may be applied to read, update, and destroy actions. For update and destroy, they apply to the data *before* the action is run. For reads, they will automatically restrict the returned data to be compliant with the filter. Expression checks, explained in more detail below, are really just Filter Checks.

```elixir
defmodule MyApp.Checks.ActorOverAgeLimit do
  use Ash.Policy.FilterCheck

  require Ash.Query
  import Ash.Filter.TemplateHelpers, only: [actor: 1]

  # A description is not necessary, as it will be derived from the filter, but one could be added
  # def describe(_opts), do: "Actor is over the age limit"

  # filter checks don't have the `context` available to them
  def filter(_options) do
    Ash.Query.expr(age_limit <= ^actor(:age))
  end
end
```

##### Expression Checks

A simple way to define a policy is by using `expr/1` in the policy. For example:

```elixir
authorize_if expr(exists(role, name == "owner"))
```

Keep in mind that, for create actions, many `expr/1` checks won't make sense, and may return `false` when you wouldn't expect. Expression (and other filter) policies apply to "a synthesized result" of applying the action, so related values won't be available. For this reason, you may end up wanting to use other checks that are built for working against changesets, or only simple attribute-based filter checks. Custom checks may also be warranted here.

###### Referencing the actor

In expression policies, the `actor` template can be used (other templates that may work in filter expressions, for example, are not available). For example:

```elixir
authorize_if expr(author.id == ^actor(:id))
```

###### Using exists

Lets compare the following expressions: 

Filtering on related data by directly referencing the relationship
```elixir
friends.first_name == "ted" and friends.last_name == "dansen"
```

Filtering on related data using `exists/2`

```elixir
exists(friends, first_name == "ted") and exists(friends, last_name == "dansen")
```

In policies (and often any time you mean "a related thing exists where some condition is true") you should generally prefer filter checks, it is advised to use `exists/2` when referring to relationships, because of the way that the policy authorizer may mix & match your policies when building filters. This is also true when adding filters to actions. If you use `exists`, then your policies can be used in filters without excluding unnecessary data, i.e `exists(friends, first_name == "ted") and exists(friends, last_name == "dansen")` means "you have one friend with the first_name "ted" and one friend with the last_name "dansen". For instance, imagine a scenario where you have an action like this:

```elixir
read :friends_of_ted do
  filter expr(friends.first_name == "ted")
end
```

And someone calls it like so:
```elixir
Resource
|> Ash.Query.for_read(:friends_of_ted)
|> Ash.Query.filter(friends.last_name == "dansen")
```

The resulting filter is `friends.first_name == "ted" and friends.last_name == "dansen"`. This means that there must be one friend with the name "ted dansen". Sometimes that *is* what you mean to do, but generally speaking I would expect the above code to say "friends of ted that also have a friend with the last name `"dansen"`". To accomplish that, we can rework the example like so:
```elixir
read :friends_of_ted do
  filter expr(exists(friends, first_name == "ted"))
end

# Calling it
Resource
|> Ash.Query.for_read(:friends_of_ted)
|> Ash.Query.filter(exists(friends, last_name == "dansen"))
```

### Access Type

The default access type is `:filter`. In most cases this will be all you need. In the example above, if a user made a request for all instances
of the resource, it wouldn't actually return a forbidden error. Instead, it attaches the appropriate filter to fetch data that the user can see.
If the actor attribute `active` was `false`, then the request _would_ be forbidden (because there is no data for which they can pass this policy). However, if `active` is `true`, the authorizer would attach the following filter to the request:

```elixir
public or owner == ^actor(:_primary_key)
```

To understand what `actor(:_primary_key)` means, see the Filter Templates section in `Ash.Filter`

Additionally, some checks have more expensive components that can't be checked before the request is run. To enable those, use the `access_type :runtime`. All checks that can be implemented as filters or strict checks will still be done that way, but this enables checks to run their `check/4` callback if necessary.

## Policy Breakdowns

## Explanation

Policy breakdowns can be fetched on demand for a given forbidden error (either an `Ash.Error.Forbidden` that contains one ore more `Ash.Error.Forbidden.Policy`
errors, or an `Ash.Error.Forbidden.Policy` error itself), via `Ash.Policy.Forbidden.Error.report/2`.

Here is an example policy breakdown from tests:

```text
Policy Breakdown
A check status of `?` implies that the solver did not need to determine that check.
Some checks may look like they failed when in reality there was no need to check them.
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
