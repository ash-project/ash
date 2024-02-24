# Atomics

Atomics allow you to attach expression-based changes to changesets, to be executed in the data layer when the action is performed.

For example:

```elixir
update :increment_score do
  argument :points, :integer, allow_nil?: false
  change atomic_update(:score, expr(score + ^arg(:points)))
end
```

## What is supported

- Atomics are only supported in update actions and upserts. In the case of upserts, the atomic changes are only applied in the case of a conflicting record.
- Attaching atomics to an action using the `atomic_update/2` change in the action, as shown in the example below.
- Attaching atomics to a changeset manually with `Ash.Changeset.atomic_update/3`
- Using calculations that don't refer to aggregates or related values in expressions

### Manually attached

```elixir
changeset
|> Ash.Changeset.atomic_update(:score, Ash.Expr.expr(score + 1))
|> Ash.update!()
```

### Upsert example

```elixir
create :upsert do
  upsert? true
  change set_attribute(:points, 1) # set to 1
  set_on_upsert :points, expr(points + 1) # or increment existing
end
```
