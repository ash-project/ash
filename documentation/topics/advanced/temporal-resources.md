<!--
SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>

SPDX-License-Identifier: MIT
-->

# Temporal Resources

> ### Experimental {: .warning}
>
> Temporal resources are experimental, and the API may change. In production they need
> `ash_postgres` running on **PostgreSQL 18+**. It's what gives us SQL:2011 application-time
> period tables: `PRIMARY KEY (... WITHOUT OVERLAPS)` and `PERIOD` foreign keys. This guide is
> written against PostgreSQL for that reason. `Ash.DataLayer.Ets` supports temporal resources
> too, in memory.

Your standard resource only knows what's true right now. Update a row, and whatever it said
before is gone. A *temporal* resource keeps all of it. Every row is valid for a *period*, a
half-open range `[from, to)`, and a single record is spread across many rows, one for each
period of its history.

Reading a temporal resource always happens at a point in time. You see the one version of each
record that was valid at that instant, and nothing else.

You might see this called an *application-time period table*. The period is your time, set by
your application, as opposed to system time, set by the database's clock.

## Defining a temporal resource

Give your resource a `temporal` section naming its **period attribute**, which is an
`Ash.Type.Range` over a datetime, and use a data layer that supports it:

```elixir
defmodule MyApp.Subscription do
  use Ash.Resource,
    domain: MyApp.Billing,
    data_layer: AshPostgres.DataLayer

  postgres do
    table "subscription"
    repo MyApp.Repo
  end

  temporal do
    strategy :context
    attribute :valid_at
  end

  attributes do
    attribute :id, :integer, primary_key?: true, allow_nil?: false, public?: true
    attribute :plan, :string, public?: true

    attribute :valid_at, Ash.Type.Range,
      allow_nil?: false,
      constraints: [
        inner_type: :utc_datetime_usec,
        lower: [inclusive?: true],
        upper: [inclusive?: false]
      ]
  end

  actions do
    # `valid_at` is intentionally NOT in `accept` — see "Writing".
    defaults [:read, :destroy, create: [:id, :plan]]
  end
end
```

You don't actually have to declare the period attribute. Leave it out, and `temporal` declares
it for you, exactly as above:

```elixir
  temporal do
    strategy :context
    attribute :valid_at
  end

  attributes do
    attribute :id, :integer, primary_key?: true, allow_nil?: false, public?: true
    attribute :plan, :string, public?: true
  end
```

Either way, it's marked `generated?`. You never pass a period in as action input. Its value
comes from the instant of the write. If you declare it yourself, it can't allow nil, and it has
to keep the `[from, to)` bounds.

The migration generator handles the table for you. It emits the period column, a
`PRIMARY KEY (id, valid_at WITHOUT OVERLAPS)` (a GiST exclusion that stops two rows of the same
`id` from overlapping in time), and installs `btree_gist`.

## Reading "as of" a point in time

Use `Ash.Query.as_of/2`, or pass `as_of` to any builder or action:

```elixir
# time travel: the subscription as it was on Jan 15
MyApp.Subscription
|> Ash.Query.filter(id == 1)
|> Ash.Query.as_of(~U[2026-01-15 00:00:00Z])
|> Ash.read!()

# `as_of` is accepted anywhere `tenant` is — opts, code interfaces, get, etc.
Ash.get!(MyApp.Subscription, 1, as_of: ~U[2026-01-15 00:00:00Z])
```

**Leave out `as_of`, and you're reading now.** You get the current state, exactly one row per
`id`, and never the full history. There's no "all of history" read. Every read is a single
point in time.

`as_of` travels the same way `tenant` does, through the shared context. Loaded relationships,
calculations, aggregates and nested actions all pick it up, so everything you get back comes
from the same moment.

### `now()` is anchored to `as_of`

Inside filters, calculations and validations, `now()`, `ago()` and `from_now()` mean the
query's `as_of`, **not** the wall clock. That's what keeps time travel consistent with itself.
Evaluate `expr(activated_at < now())` as of last year, and it compares against *last year*.
`now()` is also worked out once for the whole operation, rather than again for every expression.

## Writing

As of an instant, a temporal write makes something true *from `as_of` onward*. The new row is
`[as_of, ∞)`. As of a range, it makes it true *over that range* (see
[Writing over a period](#writing-over-a-period)). A few things follow from that:

- **You never set `valid_at` as action input.** You pass `as_of` instead, so leave the period
  attribute out of every action's `accept`. Set the instant with the `as_of` option or
  `Ash.Changeset.as_of/2`. Pass neither, and a single `now` is pinned for the whole write, so
  the period, any `&DateTime.utc_now/0` defaults (like `create_timestamp`), and the stamped
  `as_of` all share the exact same instant.
- **An update splits the period.** Updating as of an instant cuts the currently valid version
  off at that instant, and writes a new one with the new values from there **to wherever the
  old version ended**. That's only `[as_of, ∞)` when the version it split was open-ended itself.
  Split at the exact instant a version began, and there's nothing left before it, so the update
  works like an ordinary overwrite.
- **A destroy ends validity. It doesn't delete history.** Destroying as of an instant cuts the
  currently valid version down to `[lower, as_of)`. The record is gone from that instant on,
  and untouched before it. Destroy at the exact instant the version began, and it's removed
  entirely.

```elixir
# create the current version, valid from now on
MyApp.Subscription
|> Ash.Changeset.for_create(:create, %{id: 1, plan: "bronze"})
|> Ash.create!()

# "as of" March 1, change the plan: history before March 1 is preserved
sub
|> Ash.Changeset.for_update(:change_plan, %{plan: "gold"}, as_of: ~U[2026-03-01 00:00:00Z])
|> Ash.update!()
```

> ### Pass `as_of` as the action option {: .info}
>
> Some changes need the instant while the changeset is being built: cascading destroys,
> `manage_relationship`, and identity pre-checks and eager checks. For writes like those, pass
> `as_of` as the **action option** (`for_create(:create, input, as_of: ...)`), not with
> `Ash.Changeset.as_of/2` afterwards. By then it's too late.

> ### Where the split happens {: .info}
>
> SQL:2011 has a statement for exactly this: `UPDATE/DELETE ... FOR PORTION OF`. It was slated
> for PostgreSQL 19, and this feature was first built on it, but PostgreSQL reverted it before
> release: concurrent writes under `READ COMMITTED` could silently lose part of an update. So
> `ash_postgres` splits versions itself instead, in a single statement, on PostgreSQL 18, and
> makes sure concurrent writes don't lose anything. Once PostgreSQL ships `FOR PORTION OF`
> natively, `ash_postgres` will use it. See the
> [AshPostgres guide](https://hexdocs.pm/ash_postgres/temporal-resources.html) for the details.

### Writing over a period

Pass a range as `as_of`, and the write applies over that period. A create opens exactly that
period. An update or a destroy acts on each version the range overlaps, over the part of the
range that version holds. The range satisfies the period attribute's constraints, so with
`upper: [inclusive?: false]` it is written `[from, to)`:

```elixir
# a trial for the first half of 2027 only
MyApp.Subscription
|> Ash.Changeset.for_create(:create, %{id: 2, plan: "trial"},
  as_of: %Ash.Range{
    lower: ~U[2027-01-01 00:00:00Z],
    upper: ~U[2027-07-01 00:00:00Z],
    bounds: :"[)"
  }
)
|> Ash.create!()
```

### When no version is valid at that instant

An update or destroy acts on the version that's valid at `as_of`. If there isn't one, there's
nothing to split, and the write is refused as a stale record. That's true whether the last
version already ended, the only one hasn't started yet, or `as_of` falls in a gap between two.

To bring back a record whose history has run out, **create** it again rather than updating it.
The create opens a fresh `[now, ∞)` next to the closed history, which you can still read at its
own instants.

### Scheduling a change ahead of now

A create as of an instant opens `[as_of, ∞)`, so it overlaps any later version of the record.
Two writes produce a bounded period instead: a create as of a range, which opens exactly that
range, and an update, which inherits the end of the version it splits. So a change in the future
is written as an update:

```elixir
# splits into [now, 2027-01-01) and [2027-01-01, ∞)
sub
|> Ash.Changeset.for_update(:change_plan, %{plan: "gold"}, as_of: ~U[2027-01-01 00:00:00Z])
|> Ash.update!()
```

> ### A future version that was created has to be unwound {: .warning}
>
> If the later version was *created as of an instant* rather than written as an update, it holds
> `[its instant, ∞)`, and nothing can be written before it. To get out of that, destroy that
> version, create the present one, and then make the future change again as an update.

## Relationships

A `belongs_to` can point at another temporal resource *for the matching period*. Declare
`temporal_keys`, and you get a Postgres `PERIOD` foreign key:

```elixir
relationships do
  belongs_to :tier, MyApp.Tier do
    source_attribute :tier_id
    destination_attribute :id
    temporal_keys {:valid_at, :valid_at}
  end
end
```

PostgreSQL only supports `NO ACTION` on `PERIOD` foreign keys, so `on_delete` and `on_update`
referential actions are rejected at compile time. Cascade in your application instead, with
something like `change cascade_destroy(:subscriptions)`.

## Identities

On a temporal resource, identities become period-aware `UNIQUE (... WITHOUT OVERLAPS)`
exclusions rather than plain unique indexes. A plain unique index would be wrong both ways. It
would reject the second period of any record, so you couldn't have history at all. And a naive
`(email, valid_at)` index would *allow* two records to share a value at the same instant.

The period-aware version means "unique at every instant, with history allowed". You write
`identity :unique_email, [:email]` like you always do, and it does the right thing.

## Authorization

> ### The actor is taken as it is; data is read "as of" {: .warning}
>
> This is the most important thing to understand about authorizing temporal resources. A
> temporal query reads **data** as of the query's timestamp, but it takes the **actor's
> attributes** exactly as they are on the actor struct you pass in. They are *not* re-fetched
> as of the query's instant.

Policy checks fall into two camps, and they're resolved at different times:

- **Actor attribute checks**, like `actor_attribute_equals/2`, `actor_present`, and any
  expression reading `^actor(:field)`, are checked against the actor struct in memory, with
  whatever values it was loaded with.
- **Data and filter checks**, like filter policies, `relates_to_actor_via`, and expressions over
  the resource's own data, are checked **as of the query's `as_of`**.

If you load the actor now but run a query as of some other instant, those two disagree. You'd be
authorizing *historical* data with the actor's *current* attributes, or the other way around.
For example, someone who's an admin **today** passes `actor_attribute_equals(:role, :admin)`
even while reading data as of last year, when they may not have been an admin at all.

**Rule of thumb: fetch the actor as of the same instant you're about to query.** Then the
actor's attributes and the data you're reading describe the same moment:

```elixir
as_of = ~U[2026-01-15 00:00:00Z]

# load the actor AS OF the same instant as the query
actor = Ash.get!(MyApp.User, user_id, as_of: as_of)

MyApp.Subscription
|> Ash.Query.as_of(as_of)
|> Ash.read!(actor: actor)
```

Ash can't do this for you. It has no way of knowing that the actor struct you handed it was
loaded at a different point in time than the query. Keeping the actor and the query on the same
`as_of` is the only way to get authorization decisions that agree with themselves.

We'd like to add support for transparently reloading the actor as of the query's time at some
point, but that needs some new work and some new configuration.

(`Ash.can?` threads `as_of` onto the subject it builds, and policy filter checks that use
`now()` are left to the data layer, so they're evaluated at the query's instant rather than the
wall clock. Neither of those changes the fact that the **actor struct's own attributes** are
whatever you loaded.)

## Changes, validations and preparations

Every action on a temporal resource runs as of a point in time, so anything that runs as part of
one can't assume it's happening now. It can't read the wall clock (use `now()` in expressions,
or the subject's `as_of`), it can't have side effects that assume the present, and it has to do
any reads or nested actions through Ash, so that `as_of` gets passed along to them.

Changes, validations and preparations say they meet that bar with the `temporal_safe?/1`
callback of their behaviour, which defaults to `false`. Run one that hasn't said so on a
temporal resource, and you'll get `Ash.Error.Framework.NotTemporalSafe`:

```elixir
defmodule MyApp.Changes.Slugify do
  use Ash.Resource.Change

  @impl true
  def temporal_safe?(_opts), do: true

  @impl true
  def change(changeset, _opts, _context), do: # ...
end
```

If you're using changes, validations or preparations from a package that doesn't declare
`temporal_safe?/1` yet, you can mark them as temporal safe in config. This is checked at
compile time:

```elixir
config :ash, :temporal_safe_modules, [SomePackage.Changes.DoesThing]
```

The built-in changes, validations and preparations are all temporal safe (`set_attribute` with
`&DateTime.utc_now/0` resolves to the write's `as_of`, just like an attribute default). The
exceptions are the ones that wrap an arbitrary function: `before_action`, `after_action`,
`before_transaction`, `after_transaction`, and anonymous function changes, validations and
preparations. There's no way to know whether those are safe, so move that logic into a module
that declares `temporal_safe?/1` to use it on a temporal resource.

See the [changes](/documentation/topics/resources/changes.md#temporal-safety),
[validations](/documentation/topics/resources/validations.md#temporal-safety) and
[preparations](/documentation/topics/resources/preparations.md#temporal-safety) guides.

## Limitations

- **`ash_postgres` on PostgreSQL 18+, or `Ash.DataLayer.Ets`.** Every other data layer reports
  `Ash.DataLayer.can?(:temporal)` as `false`, and a resource that declares itself temporal on
  one of them won't compile. The two supported data layers behave the same on everything above,
  in different ways. Postgres keys the table `PRIMARY KEY (id, valid_at WITHOUT OVERLAPS)` and
  splits a version by writing the slice inside the write's period and putting back the rest,
  in one statement. ETS puts the period in its storage key and rewrites the versions it affects.
- **Concurrent writes to one record are retried.** Under `READ COMMITTED`, a write locks the
  versions it's about to split. If another transaction changed one of them in the meantime, the
  write does nothing and runs again. Two upserts that race to create the same record are
  retried the same way. After 25 conflicts in a row, which takes many transactions writing the
  same slice of the same record at once, the action fails with
  `AshPostgres.Temporal.WriteConflict`, and retrying it is safe. Under `REPEATABLE READ` or
  `SERIALIZABLE`, PostgreSQL raises a serialization failure instead, and retrying is up to you.
- **`Ash.DataLayer.Ets` writes aren't transactional.** A split deletes the version and then
  writes both halves, in that order, so someone reading at the same time can see the record
  missing, but never as two versions at once. Overlaps are checked, not locked, so two creates
  at the same time can both land.
- **Periods are ranges over datetimes.** A period over dates (validity tracked by the day),
  naive datetimes, or any other ordered type is refused at compile time.
- **No "all of history" reads.** Every read is a single point in time, and querying across
  several periods of the same record at once isn't supported. Doing that properly gets into
  some mind-bending, timey-wimey territory, and it's something we may take on later.
- **Manual actions skip temporal handling.** The data layer is never called, so managing periods
  is up to you.
- **No database-level referential actions on `PERIOD` foreign keys.** That's a PostgreSQL rule,
  so cascade in your application instead.
