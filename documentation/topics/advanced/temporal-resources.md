<!--
SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>

SPDX-License-Identifier: MIT
-->

# Temporal Resources

> ### Experimental {: .warning}
>
> Temporal resources are experimental and the API may change. In production
> they require `ash_postgres` running against **PostgreSQL 19+** (which provides
> SQL:2011 application-time period tables — `PRIMARY KEY (... WITHOUT OVERLAPS)`,
> `PERIOD` foreign keys, and `UPDATE/DELETE ... FOR PORTION OF`), which is still
> in beta itself. This documentation is written against PostgreSQL for that
> reason. `Ash.DataLayer.Ets` supports temporal resources too, in memory.

A *temporal* resource keeps a full history of each row over time instead of
overwriting it. Every row is valid for a *period* — a half-open range
`[from, to)` — and the table can hold many rows for the same logical record,
one per period. Reading a temporal resource is always a point in time
("as of") read: you see the single version of each record that was valid at
that instant.

This is sometimes called an *application-time period table* (the validity is
business/application time, set by you, as opposed to system/transaction time
set by the database clock).

## Defining a temporal resource

A temporal resource declares a `temporal` section naming the **period
attribute** — an `Ash.Type.Range` over a datetime — and uses a data layer that
supports it:

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
        inner_type: :datetime,
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

The period attribute may be left out entirely — `temporal` declares it for you,
exactly as above:

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

Either way it is marked `generated?`: a period is never action input, its value comes
from the instant of the write. Declare it yourself to choose the type of its bounds or
their precision — a `:date` period for validity tracked by the day, say. It must be a
range, must not allow nil, and must keep the `[from, to)` bounds.

The migration generator emits the period column, a
`PRIMARY KEY (id, valid_at WITHOUT OVERLAPS)` (a GiST exclusion that prevents
two rows of the same `id` from overlapping in time), and installs `btree_gist`.

## Reading "as of" a point in time

Use `Ash.Query.as_of/2`, or the `as_of` option on any builder/action:

```elixir
# time travel: the subscription as it was on Jan 15
MyApp.Subscription
|> Ash.Query.filter(id == 1)
|> Ash.Query.as_of(~U[2026-01-15 00:00:00Z])
|> Ash.read!()

# `as_of` is accepted anywhere `tenant` is — opts, code interfaces, get, etc.
Ash.get!(MyApp.Subscription, 1, as_of: ~U[2026-01-15 00:00:00Z])
```

**Reads default to the current instant.** If you don't provide `as_of`, a
temporal read is anchored to `now()` — you get current state, exactly one row
per `id`, never the full history. (There is no "all history" read; every read
is a single point in time.)

`as_of` propagates like `tenant` does — through the shared context — so it
flows into loaded relationships, calculations, aggregates, and nested actions
automatically.

### `now()` is anchored to `as_of`

Inside filters, calculations, and validations, `now()` / `ago()` / `from_now()`
evaluate to the query's `as_of`, **not** the wall clock. A time-travel read is
internally consistent: `expr(activated_at < now())` evaluated `as_of` last year
compares against *last year*, and `now()` is resolved once for the whole
operation rather than re-evaluated per expression.

## Writing

A temporal write establishes validity *from `as_of` onward* — the new row is
`[as_of, ∞)`. Because of this:

- **`valid_at` is never settable as action input.** You don't pass a period;
  you pass `as_of`. (Leave the period attribute out of every action's
  `accept`.) Set the instant with the `as_of` option or `Ash.Changeset.as_of/2`;
  with neither, a single `now` is pinned for the write so the period, any
  `&DateTime.utc_now/0` defaults (e.g. `create_timestamp`), and the stamped
  `as_of` all share the exact same instant.
- **An update splits the period.** Updating "as of" an instant truncates the
  currently-valid record at that instant and writes a new one carrying the new
  values from there **to wherever the old version ended** — `[as_of, ∞)` only when
  the version it split was itself open-ended. Splitting at the instant a version
  began leaves nothing before it, so that update reads as an ordinary overwrite.
- **A destroy ends validity; it does not delete history.** Destroying "as of" an
  instant truncates the currently-valid record to `[lower, as_of)`. The record is
  gone from that instant onward and unchanged before it. Destroying at the instant
  the version began removes it entirely.

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
> For writes whose *changes* need the instant — cascading destroys,
> `manage_relationship`, and identity pre/eager-checks — pass `as_of` as the
> **action option** (`for_create(:create, input, as_of: ...)`), not via
> `Ash.Changeset.as_of/2` after building the changeset. Those run while the
> changeset is being constructed, so the instant must be set before then.

### When no version is valid at that instant

An update or destroy acts on the version valid at `as_of`. When none is, there is
nothing to split and the write is refused as a stale record — whether the last
version has already ended, the only one has not begun, or `as_of` falls in a gap
between two.

A record whose history has run out is brought back by **creating** it again, not by
updating it: the create opens a fresh `[now, ∞)` alongside the closed history, which
stays readable at its own instants.

### Scheduling a change ahead of now

A create always opens `[as_of, ∞)`, so two of them always overlap and you cannot
create a record "now" while a version dated later already exists. Only an update
produces a bounded period, by inheriting the end of the version it splits, so a
future-dated change is expressed as an update:

```elixir
# splits into [now, 2027-01-01) and [2027-01-01, ∞)
sub
|> Ash.Changeset.for_update(:change_plan, %{plan: "gold"}, as_of: ~U[2027-01-01 00:00:00Z])
|> Ash.update!()
```

> ### A future version that was created has to be unwound {: .warning}
>
> If the later version was *created* rather than scheduled as an update, it holds
> `[its instant, ∞)` and nothing can be written before it. Recovering means
> destroying that version, creating the present one, and re-applying the future
> change as an update.

## Relationships

A `belongs_to` can reference another temporal resource *for the matching
period* by declaring `temporal_keys`, which produces a Postgres `PERIOD`
foreign key:

```elixir
relationships do
  belongs_to :tier, MyApp.Tier do
    source_attribute :tier_id
    destination_attribute :id
    temporal_keys {:valid_at, :valid_at}
  end
end
```

PostgreSQL only supports `NO ACTION` on `PERIOD` foreign keys — `on_delete` /
`on_update` referential actions are rejected at compile time. Handle cascades
in the application (e.g. `change cascade_destroy(:subscriptions)`).

## Identities

Identities on a temporal resource are emitted as period-aware
`UNIQUE (... WITHOUT OVERLAPS)` exclusions rather than plain unique indexes. A
plain unique index would be wrong both ways: it would reject the second period
of any record (making history impossible), while a naive `(email, valid_at)`
index would *allow* two records to share a value at the same instant. The
period-aware form means "unique at every instant, history allowed" — you write
`identity :unique_email, [:email]` as usual and it does the right thing.

## Authorization

> ### The actor is trusted as-is; data is read "as of" {: .warning}
>
> This is the most important thing to understand about authorizing temporal
> resources. A temporal query reads **data** as of the query's timestamp, but
> the **actor's attributes are taken at face value** from the actor struct you
> pass in — they are *not* re-fetched as of the query's instant.

Policy checks fall into two camps that resolve at different times:

- **Actor-attribute checks** — `actor_attribute_equals/2`, `actor_present`,
  and any expression reading `^actor(:field)` — are evaluated against the
  in-memory actor struct, using whatever values it was loaded with.
- **Data / filter checks** — filter policies, `relates_to_actor_via`, and
  expressions over the resource's own data — are evaluated **as of the query's
  `as_of`**.

If you load the actor "now" but run a query `as_of` a different instant, these
two disagree. You would authorize *historical* data using the actor's *current*
attributes (or vice versa). For example, a user who is an admin **today** passes
`actor_attribute_equals(:role, :admin)` even while reading data `as_of` last
year — when they may not have been an admin at all.

**Rule of thumb: fetch the actor as of the same instant you are about to
query**, so the actor's attributes and the queried data describe the same
moment:

```elixir
as_of = ~U[2026-01-15 00:00:00Z]

# load the actor AS OF the same instant as the query
actor = Ash.get!(MyApp.User, user_id, as_of: as_of)

MyApp.Subscription
|> Ash.Query.as_of(as_of)
|> Ash.read!(actor: actor)
```

Ash cannot do this for you — it has no way to know that the actor struct you
handed it was loaded at a different point in time than the query. Keeping the
actor and the query on the same `as_of` is the only way to get internally
consistent authorization decisions on temporal data.

At some point in the future we would like to add support for transparently "reloading"
the actor as of the query time, but this would require some new work & new configuration.

(`Ash.can?` threads `as_of` onto the subject it builds, and policy filter
checks that reference `now()` are deferred to the data layer so they're
evaluated at the query's instant rather than the wall clock — but neither of
those changes the fact that the **actor struct's own attributes** are whatever
you loaded.)

## Limitations

- **`ash_postgres` on PostgreSQL 19+, or `Ash.DataLayer.Ets`.** Every other data
  layer reports `Ash.DataLayer.can?(:temporal)` as `false`, and a resource that
  declares itself temporal on one fails to compile. The two behave the same on
  every point above, by different mechanisms: Postgres keys the table
  `PRIMARY KEY (id, valid_at WITHOUT OVERLAPS)` and splits with `FOR PORTION OF`,
  where ETS puts the period in its storage key and rewrites the affected versions.
- **`Ash.DataLayer.Ets` writes are not transactional.** A split deletes the version
  and writes both halves, in that order, so a concurrent reader can see the record
  absent but never as two versions at once. Non-overlap is checked rather than
  locked: two concurrent creates can both land.
- **No "all history" reads.** Every read is a single point in time. Querying
  across multiple periods of the same record at once is not supported.
- **Manual actions bypass temporal handling** — the data layer is never
  invoked, so it's on you to manage periods.
- **No database-level referential actions on `PERIOD` foreign keys** (a
  PostgreSQL constraint); cascade in the application instead.
