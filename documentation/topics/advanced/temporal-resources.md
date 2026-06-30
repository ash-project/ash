<!--
SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>

SPDX-License-Identifier: MIT
-->

# Temporal Resources

> ### Experimental {: .warning}
>
> Temporal resources are experimental and currently require `ash_postgres`
> running against **PostgreSQL 19+** (which provides SQL:2011 application-time
> period tables — `PRIMARY KEY (... WITHOUT OVERLAPS)`, `PERIOD` foreign keys,
> and `UPDATE/DELETE ... FOR PORTION OF`). The API may change.
> This documentation is very PostgreSQL specific given that this
> is the only target we support currently.
> PostgreSQL 19 itself is still in beta as well

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

    # The period. Never set directly — its value comes from `as_of` (see below).
    attribute :valid_at, Ash.Type.Range,
      constraints: [inner_type: :datetime, inner_constraints: [precision: :microsecond]],
      public?: true
  end

  actions do
    # `valid_at` is intentionally NOT in `accept` — see "Writing".
    defaults [:read, :destroy, create: [:id, :plan]]
  end
end
```

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
- **Updates and destroys split the period.** Updating "as of" an instant
  truncates the currently-valid row at that instant and writes a new row with
  the new values for `[as_of, ∞)`, using `FOR PORTION OF`. The prior version is
  preserved as history.

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

- **PostgreSQL 19+ via `ash_postgres` only.** Other data layers report
  `Ash.DataLayer.can?(:temporal)` as `false`.
- **No "all history" reads.** Every read is a single point in time. Querying
  across multiple periods of the same record at once is not supported.
- **Manual actions bypass temporal handling** — the data layer is never
  invoked, so it's on you to manage periods.
- **No database-level referential actions on `PERIOD` foreign keys** (a
  PostgreSQL constraint); cascade in the application instead.
