# Policies

Policies determine what actions on a resource are permitted for a given actor, and can also filter the results of read actions to restrict the results to only records that should be visible.

To restrict access to specific fields (attributes, aggregates, calculations), see the section on field policies.

Read and understand the [Actors & Authorization guide](/documentation/topics/security/actors-and-authorization.md) before proceeding, which explains actors, how to set them, and other relevant configurations.

## Setup

You'll need to add the extension to your resource, like so:

```elixir
use Ash.Resource, authorizers: [Ash.Policy.Authorizer]
```

Then you can start defining policies for your resource.

## Policies

### Anatomy of a Policy

Each policy defined in a resource has two parts -

- a condition, such as `action_type(:read)` or `actor_attribute_equals(:admin, true)` or `always()`. If this condition is true for a given action request, then the policy will be applied to the request.
- a set of policy checks, each of which will be evaluated individually if a policy applies to a request.

If more than one policy applies to any given request (eg. an admin actor calls a read action) then **all applicable policies must pass** for the action to be performed.

A policy will produce one of three results: `:forbidden`, `:authorized`, or `:unknown`. `:unknown` is treated the same as `:forbidden`.

### The Simplest Policy

Let's start with the simplest (most permissive) policy:

```elixir
policies do
  policy always() do
    authorize_if always()
  end
end
```

The first argument to `policy` is the condition. In this case, the condition is `always()` - a built-in helper always returning true, meaning that the policy applies to every request.

Within this policy we have a single policy check, declared with `authorize_if`. Checks logically apply from top to bottom, based on their check type. In this case, we'd read the policy as "this policy always applies, and authorizes always".

There are four check types, all of which do what they sound like they do:

- `authorize_if` - if the check is true, the whole policy is authorized.
- `authorize_unless` - if the check is false, the whole policy is authorized.
- `forbid_if` - if the check is true, the whole policy is forbidden.
- `forbid_unless` - if the check is false, the whole policy is forbidden.

If a single check does not explicitly authorize or forbid the whole policy, then the flow moves to the next check. For example, if an `authorize_if` check does NOT return true, this _does not mean the whole policy is forbidden_ - it means that further checking is required.

### How a Decision is Reached

**Not every check in a policy must pass!** This is described above, but is very important so another example is provided here. Checks go from top to bottom, are evaluated independently of each other, and _the first one that reaches a decision_ determines the overall _policy result_. For example:

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
authorize_if IsSuperUser # If this is true, the actor is a superuser

# None of the rest of the checks matter, even if the actor is deactivated.
forbid_if Deactivated
authorize_if IsAdminUser
forbid_if RegularUserCanCreate
authorize_if RegularUserAuthorized
```

Conversely:

```elixir
authorize_if IsSuperUser # This can be false
forbid_if Deactivated # This can be false
authorize_if IsAdminUser # If this is true, then the policy is still authorized.

# And none of these checks matter
forbid_if RegularUserCanCreate
authorize_if RegularUserAuthorized
```

### Not all policy checks have yes/no answers

This will be covered in greater detail in [Checks](#checks), but will be briefly mentioned here.

Ash provides two basic types of policy checks - _simple_ checks and _filter_ checks. Simple checks are what we commonly think of with authorization, and what the above example would suggest - is an actor allowed to perform a given operation, yes or no? But we can also use filter checks - given a list of resources, which ones is an actor allowed to perform the operation on?

Filter checks are applied to all read actions, including those generated for bulk updates and destroys.

### Bypass policies

A bypass policy is just like a regular policy, except if a bypass passes, then other policies after it _do not need to pass_. This can be useful for writing complex access rules, or for a simple rule like "an admin can do anything" without needing to specify it as part of every other policy.

### A realistic policy

In this example, we use some of the provided built-in checks.

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

## Checks

Checks evaluate from top to bottom within a policy. A check can produce one of three results, the same that a policy can produce. While checks are not necessarily evaluated in order, they _logically apply_ in that order, so you may as well think of it in that way. It can be thought of as a step-through algorithm.

For each check, starting from the top:

- Run the check.
  - If it returns `:authorized`, the policy is `:authorized`
  - If it returns `:forbidden`, the policy is `:forbidden`
  - If it returns `:unknown`, the next check down is checked

For the example from earlier:

- `authorize_if IsSuperUser`
  - If this check succeeds, it returns `:authorized`, the whole policy is `:authorized`, and checks stop running
  - If this check fails, it returns `:unknown` and the next check is checked
- `forbid_if Deactivated`
  - We only care about this result if the previous check failed, ie. the actor is not a super user.
  - If this check succeeds, it returns `:forbidden`, the whole policy is `:forbidden`, and checks stop running
  - If this check fails, it returns `:unknown` and the next check is checked
- `authorize_if IsAdminUser`
  - We only care about this result if the previous checks failed, ie. the actor is not a super user and is not deactivated.
  - If this check succeeds, it returns `:authorized`, the whole policy is `:authorized` and checks stop running.
  - If this check fails, it returns `:unknown` and the next check is checked
- `authorize_if RegularUserAuthorized`
  - We only care about this result if the previous checks failed, ie. the actor is not a super user, not deactivated and not an admin user.
  - If this check succeeds, it returns `:authorized`, the whole policy is `:authorized` and checks stop running.
  - If this check fails, it returns `:unknown`. As there are no more checks to run, the whole policy returns `:unknown`, which is treated as forbidden and the actor is not allowed to perform the action.

### Types of checks

As mentioned earlier, there are two distinct types of checks - _simple_ checks and _filter_ checks. So far we've seen examples of both - let's look in a bit more detail.

> #### Manual Checks {: .neutral}
>
> Both simple and filter checks are a subset of a third type of check - a _manual_ check - but you will almost always want to write simple or filter checks.

#### Simple checks

Simple checks are determined at the outset of a request, and can only cause a request to be authorized or forbidden. These are typically yes/no questions - is the actor an admin? Did the actor create the post they want to call the `update` action on? Is the actor old enough to drink alcohol?

You can write a simple check by creating a new module and using the `Ash.Policy.SimpleCheck` module:

```elixir
defmodule MyApp.Checks.ActorIsOldEnough do
  use Ash.Policy.SimpleCheck

  # This is used when logging a breakdown of how a policy is applied - see Logging below.
  def describe(_) do
    "actor is old enough"
  end

  # The context here may have a changeset, query, resource, and domain module, depending
  # on the action being run.
  # `match?` should return true or false, and answer the statement being posed in the description,
  # i.e "is the actor old enough?"
  def match?(%MyApp.User{age: age} = _actor, %{resource: MyApp.Beer} = _context, _opts) do
    age >= 21
  end

  def match?(_, _, _), do: false
end
```

You can then use this module as the check name, as part of a policy:

```elixir
defmodule MyApp.Beer do
  # ...

  policies do
    policy action(:drink) do
      authorize_if MyApp.Checks.ActorIsOldEnough
    end
  end

  # ...
end
```

Ash will internally convert the true/false return value from `match?/3` to a `:authorized`/`:forbidden`/`:unknown` response, depending on how the check is being run (ie. whether it's part of an `authorize_if`/`forbid_if`/etc.)

#### Filter checks

Many checks won't return a status yes/no, but instead return a "filter" to apply to a collection of data. They are most commonly used for read actions, but can be used for all types of actions.

For update and destroy actions, they apply to the data _before_ the action is run.

For read actions, they will automatically restrict the returned data to be compliant with the filter. Using the drinking example from earlier, we could write a filter check to list only users that are old enough to drink alcohol.

There are two ways to write a filter check - by creating a module and using the `Ash.Policy.FilterCheck` module, or by using inline expression syntax.

```elixir
defmodule MyApp.Checks.ActorOverAgeLimit do
  use Ash.Policy.FilterCheck

  # A description is not necessary, as it will be derived from the filter, but one could be added
  # def describe(_opts), do: "actor is over the age limit"

  # Filter checks don't have a `context` available to them
  def filter(_options) do
    expr(age_limit <= ^actor(:age))
  end
end
```

You can then use this module as the check name, as part of a policy:

```elixir
defmodule MyApp.User do
  # ...

  policies do
    policy action(:of_drinking_age) do
      authorize_if MyApp.Checks.ActorOverAgeLimit
    end
  end

  # ...
end
```

#### Inline checks

Inline checks are filter checks, but are different enough to warrant their own documentation. These are written directly in a policy, eg.

```elixir
policy action_type(:read) do
  # Allow records with the attribute `public` set to true to be read
  authorize_if attribute(:public, true)

  # Allow records with the attribute `level` less than the value of the `level`
  # argument to the action to be read
  authorize_if expr(level <= ^arg(:level))
end
```

Keep in mind that, for create actions, many `expr/1` checks won't make sense, and may return `false` when you wouldn't expect. Expression (and other filter) policies apply to "a synthesized result" of applying the action, so related values won't be available. For this reason, you may end up wanting to use other checks that are built for working against changesets, or only simple attribute-based filter checks. Custom checks may also be warranted here.

Ash also comes with a set of built-in helpers for writing inline checks - see `Ash.Policy.Check.Builtins` for more information.

##### Referencing the actor

In expression checks, the `actor` template can be used (other templates that may work in filter expressions, for example, are not available). For example:

```elixir
# Authorize records that have an author relationship with the author ID the same as the actor ID
# ie. records authored by the actor
authorize_if expr(author.id == ^actor(:id))
```

##### Using `exists`

A common mistake when using related data in filters is to be too restrictive. Imagine a scenario where you have an action like this:

```elixir
read :friends_of_ted do
  filter expr(friends.first_name == "ted")
end
```

If this was in a User resource, it would return users that have a friend with the first name "ted". So far so good. Then someone calls it like so:

```elixir
Resource
|> Ash.Query.for_read(:friends_of_ted)
|> Ash.Query.filter(friends.last_name == "dansen")
```

The resulting filter is `friends.first_name == "ted" and friends.last_name == "dansen"`- this means that you'll get users that have a friend with the full name "ted dansen". That _might_ be what you meant, but more likely you would want "users that have a friend with the first name "ted", that also have a friend with the last name 'dansen'".

To accomplish that, we can use the `exists` helper and rework the example like so:

```elixir
# There exists a friend with the first name "ted"
read :friends_of_ted do
  filter expr(exists(friends, first_name == "ted"))
end

# And there also exists a friend with the last name "dansen"
# They may be the same friend if the user is friends with Ted Dansen!
Resource
|> Ash.Query.for_read(:friends_of_ted)
|> Ash.Query.filter(exists(friends, last_name == "dansen"))
```

In policies (and often any time you mean "a related thing exists where some condition is true"), it is advised to use `exists/2` when referring to relationships because of the way that the policy authorizer may mix & match your policies when building filters. This is also true when adding filters to actions. If you use `exists`, then your policies can be used in filters without excluding unnecessary data.

## Field Policies

Field policies allow you to authorize access to specific fields via policies scoped to fields.

For example:

```elixir
field_policies do
  field_policy :role do
    authorize_if actor_attribute_equals(:role, :supervisor)
  end
end
```

If _any_ field policies exist then _all_ fields must be authorized by a field policy.
If you want a "deny-list" style, then you can add policies for specific fields.
and add a catch-all policy using the special field name `:*`. All policies that apply
to a field must be authorized.

The only exception to the above behavior is primary keys, which can always be read by everyone.

Additionally, keep in mind that adding `Ash.Policy.Authorizer` will require that all actions
pass policies. If you want to just add field policies, you will need to add a policy that allows
all access explicitly, i.e

```elixir
policies do
  policy always() do
    authorize_if always()
  end
end
```

### Using Expressions In Field Policies

Unlike in regular policies, expressions in field policies cannot refer to related entities currently (except when using exists). Instead, you will need to create aggregates or expression calculations that return the results you want to reference.

In results, forbidden fields will be replaced with a special value: `%Ash.ForbiddenField{}`.

When these fields are referred to in filters, they will be replaced with an expression that evaluates to `nil`. To support this behavior, only simple and filter checks are allowed in field policies.

## Debugging and Logging

### Policy Breakdowns

Policy breakdowns can be fetched on demand for a given forbidden error (either an `Ash.Error.Forbidden` that contains one ore more `Ash.Error.Forbidden.Policy` errors, or an `Ash.Error.Forbidden.Policy` error itself), via `Ash.Error.Forbidden.Policy.report/2`.

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

### Including in error messages

**IMPORTANT WARNING:** The following configuration should only ever be used in development mode!

For security reasons, authorization errors don't include any extra information, aside from `forbidden`. To have authorization errors include a policy breakdown (without help text) use the following config.

```elixir
config :ash, :policies, show_policy_breakdowns?: true
```

### Logging

It is generally safe to log authorization error details, even in production. This can be very helpful when investigating certain classes of issue.

To have Ash automatically log each authorization failure, use

```elixir
config :ash, :policies, log_policy_breakdowns: :error # Use whatever log level you'd like to use here
```

To have Ash log all policy breakdowns, even successful ones (this will be lots of noise, and should only be used for dev testing)

```elixir
config :ash, :policies, log_successful_policy_breakdowns: :error # Use whatever log level you'd like to use here
```
