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
- `expensive?` -> `before_action?` (on validations)
- `required?` -> `allow_nil?` (on belongs_to relationships) Be sure to flip the boolean value!!

### DSL changes

A new option has been added to the pub_sub notifier. If you are using it with phoenix, and you want it to publish a `%Phoenix.Socket.Broadcast{}` struct (which is what it used to do if you specified the `name` option with pub sub), then you'll need to set `broadcast_type :phoenix_broadcast`

### Validation Changes

`validate match/3` is now `validate match/2`. It used to accept a message as its third argument, but there is now support for setting a message on *all* validations like so:

```elxir
validate match(:attribute, ~r/regex/), message: "message"
```

### Policy Changes

When using a filter template that references the actor, it was previously acceptable for the actor to be `nil` and still have the check pass. For example, instead of:

```elixir
authorize_if expr(actor(:field) != 10)
```

you might want

```elixir
authorize_if is_nil(actor(:field))
forbid_if expr(actor(:field) != 10)
```

### Function Changes

The following functions have been moved from `Ash.Resource.Info` to `Ash.Resource`. The old functions still exist, but will warn as deprecated.

- `set_metadata/2`
- `put_metadata/3`
- `unload_many/2`
- `unload/2`
- `get_metadata/2`
- `selected?/2`

The following functions have been moved from `Ash.Api` to `Ash.Api.Info`. The old functions still exist, but will warn as deprecated.

- `resource/2`
- `resources/1`
- `registry/1`
- `allow/1`
- `timeout/1`
- `require_actor?/1`
- `authorize/1`
- `allow_unregistered?/1`

The following functions have been moved from `Ash.Notifier.PubSub` to `Ash.Notifier.PubSub.Info`. The old functions still exist, but will warn as deprecated.

- `publications/1`
- `module/1`
- `prefix/1`
- `name/1`

The following functions have been moved. The old functions still exist, but will warn as deprecated.

- `Ash.DataLayer.Ets.private?/1` -> `Ash.DataLayer.Ets.Info.private?/1`
- `Ash.DataLayer.Ets.table/1` -> `Ash.DataLayer.Ets.Info.table/1`
- `Ash.DataLayer.Mnesia.table/1` -> `Ash.DataLayer.Mnesia.table/1`
- `Ash.Registry.warn_on_empty?/1` -> `Ash.Registry.Info.warn_on_empty?/1`
- `Ash.Registry.entries/1` -> `Ash.Registry.Info.entries/1`

The following functions have been moved:

- Ash.Resource.extensions/1 -> `Spark.extensions/1`

### Expression Changes

The `has` operator has been removed from expressions. This is a holdover from when expressions only had partial support for nesting, and is unnecessary now. Now you can do `item in list` so `has` is unnecessary.

## Upgrading to 1.53

### Default actions

Before 2.0.0, a resource would automatically get the four action types defined. Now, you need to specify them using the `defaults` option. For example:

```elixir
actions do
  defaults [:create, :read, :update, :destroy]
end
```

### Primary Actions

Primary actions have been simplified for 2.0.0. If there was a single action of a given type before, it would have been marked as `primary?` automatically. Now, `primary?` actions are fully optional, although you may still want to configure them. Certain things like managing relationships can be much simpler when paired with primary actions. For a fully explicit experience everywhere, however, you may want to skip primary actions altogether. To make sure your application behaves the same, go to each of your resources and check to see if they only have one action of each type. If they do, mark that single action as `primary?`. Additionally, the `primary_actions?` option has been removed now that all primary actions are explicit.

### Ash.Error.Query.NotFound

We used to return/raise this error directly when something wasn't found, but it was the only place in the framework not using an Error Class. So if you had anything matching on `%Ash.Error.Query.NotFound{}` it should instead now match on `%Ash.Error.Invalid{errors: [%Ash.Error.Query.NotFound{}]}`.