# Upgrading

## Upgrading to 2.0.0

### Default actions

Before 2.0.0, a resource would automatically get the four action types defined. Now, you need to specify them using the `defaults` option. For example:

```elixir
actions do
  defaults [:create, :read, :update, :destroy]
end
```

### Primary Actions

Primary actions have been simplified for 2.0.0. If there was a single action of a given type before, it would have been marked as `primary?` automatically. Now, `primary?` actions are fully optional, although you may still want to configure them. Certain things like [managing relationships](managing_relationships.md) can be much simpler when paired with primary actions. For a fully explicit experience everywhere, however, you may want to skip primary actions altogether. To make sure your application behaves the same, go to each of your resources and check to see if they only have one action of each type. If they do, mark that single action as `primary?`.