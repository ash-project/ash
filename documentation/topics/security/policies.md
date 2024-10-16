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

- a condition or a list of conditions, such as `action_type(:read)`, `[action_type(:read), actor_attribute_equals(:admin, true)]` or `always()`. If the condition, or all conditions if given a list are true for a given action request, then the policy will be applied to the request.
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

### Policy with `condition` inside `do` block

A condition or a list of conditions can also be moved inside the `policy` block.

This can make a really long list of conditions easier to read.

```elixir
policies do
  policy do
    condition always()
    authorize_if always()
  end
end
```

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
    authorize_if expr(public == true)
    # if the actor is related to the data via that data's `owner` relationship, authorize
    authorize_if relates_to_actor_via(:owner)
  end
end
```

## Policy Groups

Policy groups are a small abstraction over policies, that allow you to group policies together
that have shared conditions. Each policy inside of a policy group have the same conditions as
their group.

```elixir
policies do
  policy_group actor_attribute_equals(:role, :owner) do
    policy action_type(:read) do
      authorize_if expr(owner_id == ^actor(:id))
    end

    policy action_type([:create, :update, :destroy]) do
      authorize_if expr(owner_id == ^actor(:id))
    end
  end
end
```

### Nesting Policy groups

Policy groups can be nested. This can help when you have lots of policies and conditions.

```elixir
policies do
  policy_group condition do
    policy_group condition2 do
       policy condition3 do
         # This policy applies if condition, condition2, and condition3 are all true
       end
    end
  end
end
```

### Bypasses

Policy groups can _not_ contain bypass policies. The purpose of policy groups is to make it easier to reason
about the behavior of policies. When you see a policy group, you know that no policies inside that group will
interact with policies in other policy groups, unless they also apply.

### Access Type

Policies have an "access type" that determines when they are applied. By default, `access_type` is `:filter`.
When applied to a read action, `:filter` will result in a filtered read. For other action types, the filter will be evaluated
to determine if a forbidden error should be raised.

There are three access types, and they determine the _latest point in the process_ that any check contained by a policy can be applied.

- `strict` - All checks must be applied statically. These result in a forbidden error if they are not met.
- `filter` - All checks must be applied either statically or as a filter. These result in a filtered read if they are not met, and a
  forbidden error for other action types.
- `runtime` - This allows checks to be run _after_ the data has been read. It is exceedingly rare that you would need to use this access type.

For example, given this policy:

```elixir
policy action(:read_hidden) do
  authorize_if actor_attribute_equals(:is_admin, true)
end
```

A non-admin using the `:read_hidden` action would see an empty list of records, rather than a forbidden error.

However, with this policy

```elixir
policy action(:read_hidden) do
  access_type :strict

  authorize_if actor_attribute_equals(:is_admin, true)
end
```

A non-admin using the `:read_hidden` action would see a forbidden error.

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

  def filter(_options, _authorizer, _opts) do
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
  authorize_if expr(public == true)

  # Allow records with the attribute `level` less than the value of the `level`
  # argument to the action to be read
  authorize_if expr(level <= ^arg(:level))
end
```

##### Inline checks for create actions

When using expressions inside of policies that apply to create actions, you may not reference the data being created. For example:

```elixir
policy action_type(:create) do
  # This check is fine, as we only reference the actor
  authorize_if expr(^actor(:admin) == true)
  # This check is not, because it contains a reference to a field
  authorize_if expr(status == :active)
end
```

> ### Why can't we reference data in creates? {: .info}
>
> We cannot allow references to the data being created in create policies, because we do not yet know what the result of the action will be.
> For updates and destroys, referencing the data always references the data _prior_ to the action being run, and so it is deterministic.

If a policy that applies to creates, would result in a filter, you will get a `Ash.Error.Forbidden.CannotFilterCreates` at runtime explaining
that you must change your check. Typically this means writing a custom `Ash.Policy.SimpleCheck` instead.

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

### Handling private fields in internal functions

When calling internal functions like `Ash.read!/1`, private fields will by default always be shown.
Even if field policies apply to the resource. You can change the default behaviour by setting the
`private_fields` option on field policies.

```elixir
field_policies do
  private_fields :include
end
```

The different options are:

- `:show` will always show private fields
- `:hide` will always hide private fields
- `:include` will let you to write field policies for private fields and private fields
  will be shown or hidden depending on the outcome of the policy

If you want to overwrite the default option that is `:show`, you can do that by setting a global flag:

```elixir
config :ash, :policies, private_fields: :include
```

## Debugging and Logging

### Policy Breakdowns

Policy breakdowns can be fetched on demand for a given forbidden error (either an `Ash.Error.Forbidden` that contains one ore more `Ash.Error.Forbidden.Policy` errors, or an `Ash.Error.Forbidden.Policy` error itself), via `Ash.Error.Forbidden.Policy.report/2`.

Additionally, you can request that they be provided in the error message for all raised forbidden errors (without the help text), by setting

```elixir
config :ash, :policies, show_policy_breakdowns?: true
```

Here is an example policy breakdown from tests.

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
