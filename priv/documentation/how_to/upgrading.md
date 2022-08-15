# Upgrading

## Upgrading to 2.0

### New DSL tooling

The DSL tooling has been moved out of the Ash name space and into a more generalized tool called `Spark`. If you have written your own extensions, you will need
to refer to those modules. They are all the same, they simply have different names. You will get compiler errors/warnings on the modules you need to change, for example: `Ash.Dsl` -> `Spark.Dsl` and `Ash.Dsl.Transformer` -> `Spark.Dsl.Transformer`. One exception, `Ash.Error.Dsl.DslError`, has been changed to `Spark.Error.DslError`.

## DSL name changes

These should all be straight forward enough to do a simple find and replace in your resources.

- `source_field` -> `source_attribute`
- `destination_field` -> `destination_attribute`
- `define_field?` -> `define_attribute?`
- `field_type` -> `attribute_type`
- `source_field_on_join_table` -> `source_attribute_on_join_resource`
- `destination_field_on_join_table` -> `destination_attribute_on_join_resource`
- `no_fields?` -> `no_attributes?`

## Upgrading to 1.53

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