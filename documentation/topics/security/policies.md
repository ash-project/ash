<!--
SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>

SPDX-License-Identifier: MIT
-->

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

### Read Actions and Filtering Behavior

An important characteristic of read actions is that, by default, they are **filtered** by policies rather than returning authorization errors. This means:

- When a user is not allowed to see certain records, those records are simply filtered out of the results
- Instead of receiving a `Forbidden` error, users typically get a `NotFound` error (for single record queries) or an empty/reduced result set (for multi-record queries)
- This filtering behavior applies to all read actions, including `get`, `read`, and any custom read actions you define

For example, if a policy restricts users to only see their own posts, a query for all posts will automatically filter to only return the current user's posts, rather than raising an authorization error. Similarly, attempting to fetch a specific post that belongs to another user will result in a `NotFound` error rather than `Forbidden`.

This design is a security feature that prevents **enumeration attacks** and **information disclosure**. By not distinguishing between "record doesn't exist" and "record exists but you can't access it", the system prevents attackers from probing to discover the existence of protected data, mapping out the system's data structure, or conducting reconnaissance attacks through systematic querying.

#### Bypassing this behavior

You can bypass this behavior on a case-by-case basis with the `authorize_with` option, for data layers that support error expressions (all of the core ones except `AshSqlite`).

For example, given a post that the user cannot see:

```elixir
Ash.get!(Post, 123)
# * Invalid:
# not found

Ash.get!(Post, 123, authorize_with: :error)
# * Forbidden
```

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

    policy action_type([:update, :destroy]) do
      authorize_if expr(owner_id == ^actor(:id))
    end

    policy action_type(:create) do
      authorize_if relating_to_actor(:owner)
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
When applied to a read action, `:filter` will result in a filtered read. For update and destroy actions, the
filter is evaluated against the original loaded record, producing a forbidden error if it does not match.
For create actions, the filter is evaluated against the inserted record _inside the action's transaction_;
if it does not match, the transaction is rolled back and the action returns a forbidden error (see
[Filter checks on create actions](#filter-checks-on-create-actions) below).

There are three access types, and they determine the _latest point in the process_ that any check contained by a policy can be applied.

- `strict` - All checks must be applied statically. These result in a forbidden error if they are not met.
- `filter` - All checks must be applied either statically or as a filter. For reads they produce a filtered
  read; for updates/destroys they're evaluated against the original record; for creates they're evaluated
  post-insert inside the action's transaction.
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

### Filter checks on create actions

Filter-mode policy checks that reference data — for example, a check that requires the inserted post's
parent organization to be owned by the actor — cannot be evaluated at pre-flight time on a create action,
because the row does not exist yet. Ash handles this by deferring the check to a post-insert authorization
step inside the action's transaction:

```elixir
policy action_type(:create) do
  authorize_if expr(organization.owner_id == ^actor(:id))
end
```

When the create runs, Ash inserts the row, runs the user's `after_action` hooks, then evaluates the filter
as a single `SELECT ... WHERE pkey = ^inserted.id AND ^filter` inside the same transaction. If the row
matches, the transaction commits. If it does not match, the transaction is rolled back and the action
returns `Ash.Error.Forbidden`. There is no need to write a custom check or restructure the policy.

This behavior is automatic and requires no opt-in for the common case. Two requirements:

1. **The data layer must support transactions.** ETS-backed resources, for example, cannot participate
   because there's no transaction to roll back; Ash raises `Ash.Error.Forbidden.CannotFilterCreates` at
   pre-flight time with a message explaining why.
2. **The action must run in a transaction.** The default for create actions is `transaction? true`. If an
   action is explicitly marked `transaction? false`, the same pre-flight error fires.

#### `before_transaction` / `around_transaction` hooks require opt-in

`before_transaction` and `around_transaction` hooks run _outside_ the data-layer transaction, so any side
effects they perform (sending external notifications, writing to other systems) cannot be rolled back if
the post-insert filter rejects the row. For actions that have these hooks, Ash refuses to defer the
filter check by default — the safer behavior is to fail at pre-flight rather than fire a hook with
side effects on a create that will be rejected.

To opt in to the trade-off, set `allow_post_action_authorization? true` on the create action:

```elixir
create :create do
  allow_post_action_authorization? true
end
```

With this option set, the action proceeds: `before_transaction` runs, the row inserts, `after_action`
runs, the filter is evaluated, and the transaction commits or rolls back. The `before_transaction`
side effects will have already fired regardless of the outcome — you are acknowledging this.

#### Bulk creates

The post-insert authorization fires for `Ash.bulk_create/4` too, per record. Note that
`Ash.bulk_create(..., transaction: false)` is incompatible with filter-mode checks on create — the
hook will raise `CannotFilterCreates` at runtime because rollback is not possible without a transaction.
Use `transaction: :batch` or `transaction: :all` instead, or rewrite the policy to not need a filter
on this action.

#### Avoiding the post-insert query

The post-insert authorization adds one `SELECT` per create. For most policies this is fine — it's the
same shape of query the read path would issue, and it runs in the same transaction as the insert. But
on hot create paths it can be worth replacing the filter check with a check that reads the changeset
directly, eliminating the round-trip.

Filters that reference **only the inserted record's own attributes** can almost always be expressed as
a check against the changeset:

```elixir
# Adds a post-insert SELECT:
policy action_type(:create) do
  authorize_if expr(status == :active and visibility == :public)
end

# Equivalent, no extra query — evaluated against the changeset at pre-flight:
policy action_type(:create) do
  authorize_if changing_attributes(status: [to: :active])
  authorize_if changing_attributes(visibility: [to: :public])
end
```

For policies that reference **foreign-key attributes plus the actor**, you can often combine an
attribute check with an actor check to avoid loading the related record:

```elixir
# Adds a post-insert SELECT that joins through `organization`:
policy action_type(:create) do
  authorize_if expr(organization.owner_id == ^actor(:id))
end

# Equivalent for the common case where the actor's `organization_id` is on the actor itself —
# no query, evaluated at pre-flight:
policy action_type(:create) do
  authorize_if expr(organization_id == ^actor(:organization_id))
end
```

When the relationship truly needs to be loaded to evaluate the check (e.g. the actor doesn't carry
the join key, or the policy depends on a column that isn't a foreign key on the changeset), the
post-insert SELECT is the right tool — there's no cheaper way to verify a property of the database
that the action doesn't otherwise read. Treat the rewrites above as targeted optimizations for
measured-hot create paths, not as the default style.

### Custom error messages

Policies can carry an `error_message` that surfaces when the policy is the
reason a request is forbidden. Two forms:

```elixir
policy action(:edit_billing) do
  error_message "only admins can edit billing"
  authorize_if actor_attribute_equals(:is_admin, true)
end
```

```elixir
policy action(:edit_billing) do
  error_message fn _subject, context ->
    "actor #{inspect(context.actor.id)} can't use #{inspect(context.action.name)}"
  end

  authorize_if actor_attribute_equals(:is_admin, true)
end
```

The function form receives `(subject, context)`. `subject` is the changeset,
query, or action input being authorized. `context` is a map of `:resource`,
`:action`, `:actor`, `:domain`, and `:tenant`.

The function may also return an `Exception.t()`, which **replaces** the
default `Ash.Error.Forbidden.Policy` entirely:

```elixir
defmodule MyApp.Errors.SubscriptionRequired do
  use Splode.Error, fields: [:plan], class: :forbidden
  def message(error), do: "subscription #{inspect(error.plan)} required"
end

policy action(:premium_feature) do
  error_message fn _subject, context ->
    MyApp.Errors.SubscriptionRequired.exception(plan: context.actor.plan)
  end

  authorize_if actor_attribute_equals(:plan, :pro)
end
```

#### How the responsible policy is chosen

When multiple policies could be responsible for a denial, Ash picks one by
re-evaluating each policy's check chain against the already-computed facts:

1. Bypass policies are skipped — they don't deny on their own.
2. Among non-bypass policies whose condition applies, the first one whose
   check chain decided to `:forbidden` (explicit `forbid_if`/`forbid_unless`
   fired) wins.
3. Otherwise, the first one whose check chain ended in `:unknown` (no
   `authorize_if` matched) wins.
4. If no policy is responsible — e.g. authorization was filtered to empty —
   the default forbidden message is used.

The SAT solver only computes facts it needs to decide the request. Once one
policy fails to authorize, later policies may have facts that were never
computed; their state appears as `:unknown` and explicit-forbid preference
can't fire for them. In practice this means: when multiple policies can each
deny, the **first** one (in source order) tends to be the responsible one.
If you need a specific policy's message to win, put it first.

#### Where the message appears

For the string-returning case, the `error_message` becomes the top-level
message of the returned `Ash.Error.Forbidden.Policy`. The policy breakdown
(printed when `config :ash, policies: [show_policy_breakdowns?: true]`) is
appended below the custom message, so you keep both the user-facing
explanation and the diagnostic detail.

For the exception-returning case, your exception struct replaces the entire
`Ash.Error.Forbidden.Policy` and is added to the wrapping `Ash.Error.Forbidden`'s
`errors` list. The policy breakdown is **not** generated — your exception is
the whole story.

### Relationships and Policies

A common point of confusion when working with relationships is when they return less results or no results due to policies.
Additionally, when requesting related data that produces a forbidden error, it forbids the *entire request*.

For example, if you have a `Post`, that `belongs_to` `:author`, and the user requesting data cannot see the `author` due to a **filter** policy,
then you may see something like this:

```elixir
%MyApp.Post{author: nil, ...}
```

Even though it is not possible for a `Post` to exist without an associated `:author`!

Additionally, if the user cannot read the `author` due to a `:strict` policy, if you attempt to load the `:author`, the result
of the **entire operation** will be `{:error, %Ash.Error.Forbidden{}}`.

There are two ways that you can improve this behavior

#### The `allow_forbidden_field?` Option

This option will **default to `true`** in 4.0. You can adopt this behavior now with the following configuration.

```elixir
config :ash, :allow_forbidden_field_for_relationships_by_default?, true
```

This option adjusts the relationship reading logic such that, if running a related read action would produce a
forbidden error, the relationship will be set to `%Ash.ForbiddenField{}`, instead of forbidding the entire request.

So in the example above where the **entire operation** fails, you would instead get:

```elixir
{:ok, %MyApp.Post{author: %Ash.ForbiddenField{}}}
```

#### The `authorize_read_with` Option

This option typically only makes sense to apply on `has_one` and `belongs_to` relationships. This alters the behavior
of policy filtering when loading related records. In our above example, lets say there is a policy like the following
on `MyApp.Author`, that prevents us from reading an author that has been deactivated.

```elixir
policy action_type(:read) do
  access_type :filter # This is the default access type. It is here for example.
  authorize_unless expr(active == false)
end
```

When running a normal read action against that resource, you want any deactivated authors to be filtered out.
However, when reading the `:author` relationship, you don't want the author to appear as `nil`. This is especially
useful when combined with `allow_forbidden_field? true`.

So lets make our `belongs_to` relationship looks like this.

```elixir
belongs_to :author, MyApp.Author do
  allow_nil? false
  allow_forbidden_field? true
  authorize_read_with :error
end
```

Now, that filter will be applied in such a way that produces an error if any record exists that matches `active == false`.

So a forbidden read of the `:author` relationship will never produce a `nil` value, nor will it produce an `{:error, %Ash.Error.Forbidden{}}`
result. Instead, the value of `:author` will be `%Ash.ForbiddenField{}`!


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

For create actions, they are evaluated against the inserted row inside the action's transaction — see
[Filter checks on create actions](#filter-checks-on-create-actions).

For read actions, they will automatically restrict the returned data to be compliant with the filter. Using the drinking example from earlier, we could write a filter check to list only users that are old enough to drink alcohol.

There are two ways to write a filter check - by creating a module and using the `Ash.Policy.FilterCheck` module, or by using inline expression syntax.

```elixir
defmodule MyApp.Checks.ActorOverAgeLimit do
  use Ash.Policy.FilterCheck

  # A description is not necessary, as it will be derived from the filter, but one could be added
  # def describe(_opts), do: "actor is over the age limit"

  def filter(actor, _authorizer, _opts) do
    expr(age_limit <= ^actor.age)
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

Inline filter checks on create actions are evaluated post-insert, inside the action's transaction.
The row is inserted, the user's `after_action` hooks run, then Ash issues a single
`SELECT WHERE pkey = ^inserted.id AND ^filter` and authorizes only if the row comes back. If it does
not, the transaction is rolled back and the action returns `Ash.Error.Forbidden`. This means you can
reference attributes _and relationships_ of the inserted record:

```elixir
policy action_type(:create) do
  # Evaluated against the inserted row's `status` field
  authorize_if expr(status == :active)
  # Evaluated against the inserted row's organization relationship
  authorize_if expr(organization.owner_id == ^actor(:id))
end
```

See [Filter checks on create actions](#filter-checks-on-create-actions) above for the full mechanics,
data-layer requirements, and the `allow_post_action_authorization?` opt-in for actions that have
`before_transaction` / `around_transaction` hooks.

If the deferral path cannot be set up — the data layer does not support transactions, the action sets
`transaction? false`, or the action has non-transactional hooks without
`allow_post_action_authorization? true` — Ash raises `Ash.Error.Forbidden.CannotFilterCreates` at
pre-flight with a message explaining the specific cause.

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

## References to related data & fields

### Calculations

The dependencies of a calculation do not have any authorization applied to them. This includes the dependencies loaded with `c:Ash.Resource.Calculation.load/3`, as well as any dependencies referenced in a calculation expression. To understand why this is the case, take the following calculation:

```elixir
calculate :users_ssn_last_4, :string, expr(fragment("RIGHT(?, 4)", user.ssn))
```

The `user` record may not be visible to the actor loading this calculation, and if it is, the `ssn` field may not be visible due to field policies. If calculations authorized access to their dependencies, you would *never* be able to write a calculation like the above.

#### Aggregates

Aggregates are the one exception to the above. Aggregates referenced *anywhere* will *always* authorize access to the related data they reference (unless `authorize?: false` is set when running the action). They will not, however, authorize references to the
actual field that they refer to. For example, you may not allow managers to see the email
of their team members, but they are allowed to know something like "how many team members
have set up their email". So in an aggregate like the following:

```elixir
aggregate :users_with_email, :count, :email
```

if the field policies were applied to `:email`, that number would always show `0`,
because all `:email` attributes would appear to be `nil` to the manager.

### Fields annotated as `input`

When you use `Ash.Query.filter_input` or `Ash.Query.sort_input` (which extensions like `AshGraphql` and `AshJsonApi` do when filters/sorts are provided by the user), the contained field references are annotated as `input`. The following rules are honored for field references annotated as `input` only. Per the above section, the *contents* of a calculation's expression referenced in filters are never annotated as `input`, although the calculation itself will be, so that you can write field policies on calculations.

#### Relationship paths in filters

When a field is referenced at a relationship path, the authorization rules for that related resource's primary read action (or the relationship's configured `:read_action`) will be applied to the filter as well. For example, say that you have a `User` resource, and a `Post` resource. The `User` resource can only be read by the actor if their id matches the user's id (i.e they can only read themselves).

These policies might look like this:

```elixir
# on `User`
policy action_type(:read) do
  authorize_if expr(id == ^actor(:id))
end
```

Given a filter like `Ash.Query.filter_input(Post, %{user: %{email: "example@example.com"}})`, the resulting filter expression would be:

```elixir
expr(
  user.email == "example@example.com" and
    user.id == ^actor(:id)
)
```

This prevents a common security vulnerability that would allow malicious actors to "sniff" related data by providing filters over `User`, and seeing what `Post` records are returned.

#### Field references in filters & sorts

When a field is referenced in filters or sorts, the field reference is replaced with a conditional, that evaluates to the field value if the actor is authorized to view the field, or `nil` otherwise (causing all conditions to evaluate to `false`).

Lets say that users have a field policy that only allows viewing the email address if the user's id matches the actor's id, similar to the above example.

```elixir
field_policies do
  field_policy :email, always() do
    authorize_if expr(user.id == ^actor(:id))
  end
end
```

When that field is referenced in filters or sorts, like so: `Ash.Query.filter_input(Post, %{user: %{email: "example@example.com"}})`, the resulting expression would become:

```elixir
expr(
  if user.id == ^actor(:id) do
    user.email
  else
    nil
  end == "example@example.com"
)
```

This prevents the same security vulnerability as described above, which would allow malicious actors to "sniff" field values that should not be visible to them by providing filters and/or sorts that reference that field.

## Debugging and Logging

### Policy Breakdowns

Policy breakdowns can be fetched on demand for a given forbidden error (either an `Ash.Error.Forbidden` that contains one or more `Ash.Error.Forbidden.Policy` errors, or an `Ash.Error.Forbidden.Policy` error itself), via `Ash.Error.Forbidden.Policy.report/2`.

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

#### Global Logging Configuration

To have Ash automatically log each authorization failure, use

```elixir
config :ash, :policies, log_policy_breakdowns: :error # Use whatever log level you'd like to use here
```

To have Ash log all policy breakdowns, even successful ones (this will be lots of noise, and should only be used for dev testing)

```elixir
config :ash, :policies, log_successful_policy_breakdowns: :error # Use whatever log level you'd like to use here
```

#### Per-Request Logging

You can also enable logging for individual authorization checks by using the `log?` option with `Ash.can/3` and `Ash.can?/3`:

```elixir
# Log the authorization result for this specific check
Ash.can?(Post, :read, current_user, log?: true)

# This will log at info level when authorization succeeds or fails
MyDomain.can_read_post?(current_user, post, log?: true)
```

When `log?: true` is set, authorization results will be logged at the `:info` level regardless of the global logging configuration. This is useful for debugging specific authorization issues without enabling verbose logging globally.
