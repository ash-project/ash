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

Primary actions have been simplified for 2.0.0. If there was a single action of a given type before, it would have been marked as `primary?` automatically. Now, `primary?` actions are fully optional, although you may still want to configure them. Certain things like [managing relationships](managing_relationships.md) can be much simpler when paired with primary actions. For a fully explicit experience everywhere, however, you may want to skip primary actions altogether. To make sure your application behaves the same, go to each of your resources and check to see if they only have one action of each type. If they do, mark that single action as `primary?`. Additionally, the `primary_actions?` option has been removed now that all primary actions are explicit.

### Ash.Error.Query.NotFound

We used to return/raise this error directly when something wasn't found, but it was the only place in the framework not using an Error Class. So if you had anything matching on `%Ash.Error.Query.NotFound{}` it should instead now match on `%Ash.Error.Invalid{errors: [%Ash.Error.Query.NotFound{}]}`.