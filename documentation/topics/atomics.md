# Atomics

Atomics allow you to attach expression-based changes to changesets, to be executed in the data layer when the action is performed.

For example:

```elixir
update :increment_score do
  argument :points, :integer, allow_nil?: false
  change atomic_update(:score, expr(score + ^arg(:points)))
end
```

## Current State
Atomics are new, and we will be progressively enhancing various features to support/be aware of atomics. Unless listed below, no other features are aware of atomics. There are many places that can be enriched to either be aware of or leverage atomics. For example, changes could have an atomic and a non-atomic version, policies could be made to support atomics by altering atomic expressions to raise errors, allowing for authorization of atomic changes that doesn't have to wait until after the query.


## What is supported

- Atomics are only supported in update actions *upserts are not supported yet*
- Attaching atomics to an action using `set/2` in the action, as shown in the example below.
- Attaching atomics to a changeset by hand
- Using calculations that don't refer to aggregates in expressions

```elixir
changeset
|> Ash.Changeset.atomic_update(:score, Ash.Expr.expr(score + 1))
|> Api.update!()
```

## What is not supported/may come in the future

- atomic support in upserts, with a special reference to the row being overwritten:

```elixir
create :upsert do
  upsert? true
  change set_attribute(:points, 1) # set to 1
  set_on_upsert :points, expr(base.points + 1) # or increment existing
end
```

- using calculations that refer to aggregates/would need to join to other resources in atomics

- lowering validations, policies, and changes into atomics when data layers support it

- bulk updates using atomics, i.e

```elixir
Resource
|> Ash.Query.for_read(:some_read_action)
|> Api.update(set: [points: expr(points + 1)])
```
