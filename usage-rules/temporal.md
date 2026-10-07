<!--
SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>

SPDX-License-Identifier: MIT
-->

# Temporal Resources

Temporal resources are experimental. A temporal resource keeps every version of a record, and each version is valid for a half-open period `[from, to)`. Every read and write happens "as of" one instant. Read the "Temporal Resources" guide before working with them.

## Supported Data Layers

- Only `AshPostgres.DataLayer` on PostgreSQL 18+ and `Ash.DataLayer.Ets` support temporal resources. On any other data layer, a resource that declares `temporal` won't compile.
- Manual actions skip temporal handling entirely. If you write one, you manage periods yourself.

## Defining a Temporal Resource

```elixir
temporal do
  strategy :context
  attribute :valid_at
  # optional: wall-clock time each version was written
  recorded_at :recorded_at
end
```

- You don't have to declare the period attribute. If you leave it out, `temporal` adds an `Ash.Type.Range` over `:utc_datetime_usec` with `[from, to)` bounds.
- If you do declare it, it must be an `Ash.Type.Range` over `:utc_datetime_usec`, `:utc_datetime` or `:datetime`, with `allow_nil? false`, an inclusive lower bound and an exclusive upper bound. Ranges over dates, naive datetimes or other types are refused at compile time.
- **Never put the period attribute in an action's `accept`**, and never set it in a change. Its value always comes from the write's `as_of`.
- Use `mix ash.codegen` for migrations. It generates the `PRIMARY KEY (id, valid_at WITHOUT OVERLAPS)` and installs `btree_gist`. Don't write these by hand.

## Reading

- To read at a point in time, use `Ash.Query.as_of/2` or pass `as_of:` anywhere `tenant:` is accepted (`Ash.read`, `Ash.get`, code interfaces, `for_read`, and so on).
- If you leave out `as_of`, the read happens now and returns exactly one row per record. It never returns the full history.
- Every read is a single instant. A range-valued `as_of` on a read raises `Ash.Error.Query.AsOfNotAnInstant`. Don't try to fetch several versions of a record in one query.
- `as_of` propagates like `tenant`. Loaded relationships, calculations, aggregates and nested actions all run at the same instant, so you don't need to pass it to them again.
- In expressions, `now()`, `ago/2` and `from_now/2` mean the query's `as_of`, not the wall clock. Write `expr(expires_at > now())`, not `^DateTime.utc_now()`. Pinning the wall clock breaks time travel.

```elixir
Ash.get!(MyApp.Subscription, 1, as_of: ~U[2026-01-15 00:00:00Z])

MyApp.Subscription
|> Ash.Query.filter(plan == "gold")
|> Ash.Query.as_of(~U[2026-01-15 00:00:00Z])
|> Ash.read!()
```

## Writing

- A write makes something true from `as_of` onward. If you don't pass `as_of`, it uses now. One instant is pinned for the whole write, and `&DateTime.utc_now/0` defaults and `set_attribute(..., &DateTime.utc_now/0)` resolve to that same instant. Back-dated writes therefore get back-dated timestamps. Use `recorded_at` if you need the actual time a version was written.
- **Pass `as_of` as an action option** (`for_create(:create, input, as_of: ...)`, `for_update(...)`, `for_destroy(...)`). `Ash.Changeset.as_of/2` is applied too late for cascading destroys, `manage_relationship`, and identity pre-checks.
- A **create** opens `[as_of, ∞)`. You can't create a record if any later version of it already exists.
- An **update** splits the version valid at `as_of`. The new values run from `as_of` to wherever that version ended. To schedule a future change, write an update with a future `as_of`, not a create.
- A **destroy** closes the version valid at `as_of` down to `[from, as_of)`. It doesn't delete history.
- An update or destroy with no version valid at `as_of` (because the history ended, the record hasn't started yet, or `as_of` falls in a gap) fails as a stale record. To bring back a record whose history has ended, **create** it again rather than updating it.
- On Postgres, concurrent writes to the same record are retried automatically, up to 25 conflicts, after which `AshPostgres.Temporal.WriteConflict` is raised. It's safe to retry. Under `REPEATABLE READ`/`SERIALIZABLE` you get a serialization failure instead, and retrying is up to you.

## Changes, Validations and Preparations

- Any change, validation or preparation that runs on a temporal resource must declare `temporal_safe?/1` returning `true`. Otherwise the action raises `Ash.Error.Framework.NotTemporalSafe`. The callback defaults to `false`.
- Only declare a module temporal safe if it never reads the wall clock (use `now()` in expressions or the subject's `as_of` instead), has no side effects that assume the present, and does all of its reads and nested actions through Ash so `as_of` propagates.
- The built-in changes, validations and preparations are temporal safe. These aren't, because they wrap arbitrary functions: `before_action`, `after_action`, `before_transaction`, `after_transaction`, and anonymous-function changes, validations and preparations. Move that logic into a module that declares `temporal_safe?/1`.
- For third-party modules that you've verified are safe but that don't declare the callback, add them to `config :ash, :temporal_safe_modules, [...]`. The list is read at compile time.

```elixir
defmodule MyApp.Changes.Slugify do
  use Ash.Resource.Change

  @impl true
  def temporal_safe?(_opts), do: true

  @impl true
  def change(changeset, _opts, _context) do
    # ...
  end
end
```

## Relationships and Identities

- To make a `belongs_to` point at another temporal resource for the matching period, declare `temporal_keys {:valid_at, :valid_at}`. On Postgres this produces a `PERIOD` foreign key.
- `PERIOD` foreign keys only support `NO ACTION`, so `on_delete`/`on_update` referential actions are rejected at compile time. Cascade in the application instead, for example with `change cascade_destroy(:subscriptions)`.
- Write identities as usual (`identity :unique_email, [:email]`). On a temporal resource they become "unique at every instant" (`UNIQUE (... WITHOUT OVERLAPS)`). Don't add the period attribute to an identity.

## Authorization

- Data and filter checks are evaluated as of the query's `as_of`. Actor attribute checks (`actor_attribute_equals`, `^actor(:field)`) use the actor struct exactly as you loaded it. Ash does not re-fetch the actor at the query's instant.
- **Load the actor as of the same instant you query.** Otherwise you're authorizing historical data with the actor's current attributes.

```elixir
as_of = ~U[2026-01-15 00:00:00Z]
actor = Ash.get!(MyApp.User, user_id, as_of: as_of)

MyApp.Subscription
|> Ash.Query.as_of(as_of)
|> Ash.read!(actor: actor)
```
