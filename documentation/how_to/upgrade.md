# Upgrade

## Upgrading to 3.0

This section contains each breaking change, and the steps required to address it in your application

### DSL Changes

* `code_interface.define_for` is now `code_interface.api`. Additionally, it is set automatically if the `api` option is specified on `use Ash.Resource`.

### `Ash.Registry` has been removed

`Ash.Registry` is no longer needed. Place each resource in the api instead.

```elixir
resources do
  resource Resource1
  resource Resource2
end
```

### the `Api` of a resource must now be known when constructing a changeset, query or action input

In order to honor rules on the `Api` module about authorization and timeouts, we have to know the `Api` when building the changeset.

#### What you'll need to change

##### Embedded Resources

The api for the calls to embedded resources is gotten from the parent changeset. No need to change them at all. an `api` constraint
has been added in case you wish to make a given embedded resource use a specific api always.

For example:

```elixir
attribute :bio, MyApp.Bio do
  constraints api: MyApp.SomeApi
end
```

##### Single Api resources

While it is possible for resources to be used with multiple APIs, it almost never happens in practice. Any resources that are only used from a single 
api only (*not* including embedded resources) should be modified to have an `api` option specified in their call to `use Ash.Resource`. For example:

```elixir
use Ash.Resource,
  api: MyApp.MyApi
```

##### Multi Api resources

For these, you will need to include the `api` option every time you construct a changeset.

For example:

```elixir
MyResource
|> Ash.Changeset.for_create(:create, input, api: MyApp.MyApi)
```

### `Api.authorization.authorize` now defaults to `:by_default`

Previously, the default was `:when_requested`. This meant that, unless you said `actor: some_actor` or `authorize?: true`, authorization was skipped. This has the obvious drawback of making it easy to accidentally bypass authorization unintentionally. In 3.0, this now defaults to `:by_default`.

#### What you'll need to change

##### Keep old behavior

To avoid making a significant refactor, and to keep your current behavior, you can go to your api and set the configuration below. Otherwise skip to the refactor steps below. We advise that you take this route to start, but we *highly suggest* that you change your apis to `authorize :by_default` in the future. `authorize :when_requested` will not be deprecated, so there is no time constraint.

```elixir
authorization do
  authorize :when_requested
end
```

##### Refactor

For each api that has the old configuration, after setting it to the new config, you'll need to revisit each call to that api that doesn't set an actor or the `authorize?` option, and add `authorize?: false`.

This may be a good time to do the refactor from `YourApi.func` to `Ash.func`, if you want to.

### `require_atomic?` defaults to `true` 

On `:update` actions, and `:destroy` actions, they now default to `require_atomic? true`. This means that the following things will cause errors when attempting to run the action:

1. changes or validations exist that do not have the `atomic` callback. *This includes anonymous function changes/validations*.
2. attributes are being changed that do not support atomic updates. This most notably includes (for now) embedded resources.
3. the action has a manual implementation
4. the action has applicable notifiers that require the original data.

Updates and destroys that can be made fully atomic are always safe to do concurrently, and as such we now require that actions meet this criteria. See the [atomics guide](/documentation/topics/atomics.md) for more.

#### What you'll need to change

The vast majority of cases will be caught by warnings emitted at compile time. If you are using `change atomic_update/2` or `Ash.Changeset.atomic_update/2` or `Ash.Changeset.atomic_update/3`, and the type does not support atomic updates, you will get an error unless you do one of the following:

1. for `change atomic_update/2` add the `cast_atomic?: false` option.
2. for `Ash.Changeset.atomic_update`, pass the value as `{:atomic, expr}`, i.e `Ash.Changeset.atomic_update(changeset, :value, {:atomic, expr(value + 1)})`

For builtin types, the above applies to `:union`, `:map`, `:keyword`, embedded types. It also applies to `:string`, but only if the `match?` constraint is present.

### `Ash.Error.Invalid.NoSuchInput` errors on unknown action inputs

In 2.0, inputs to actions that don't match an accepted attribute or argument were silently ignored. This made it very easy to make certain kinds of mistakes, like assuming that an input is being used by an action when it actually is not. Now, unknown action inputs will cause an `Ash.Error.Invalid.NoSuchInput`. 

#### What you'll need to change

If you have action calls that are erroneously passing in extra values, you will need to do remove them.

A logic error was fixed in this behavior for embedded resources. If you are using embedded resources in `{:array, _}` types, and are relying on including the primary key of that embedded resource to match records up for updating/destroy behavior, you will need to make sure that you do one of the following

1. add the `writable?: true` flag to the uuid of the embedded resource (probably what you want)
2. modify the actions to accept an `id` argument and set the argument to the provided value

### `%Ash.NotLoaded{}` for attributes

In 2.0, attributes that were not selected were replaced with `nil` values. This could lead to confusion when dealing with records that didn't have all attributes selected. If you passed these records to a function it might see that an attribute is `nil` when actually it just wasn't selected. To find out if it was selected, you could look into `record.__metadata__.selected`, but you'd have to know to do that.  To alleviate these issues, attributes that are not selected are now filled in with `%Ash.NotLoaded{}`, just like calculations and aggregates.

#### What you'll need to change

If you have logic that was looking at attribute values that may not be selected, you may have been accidentally working with non selected values. For example:

```elixir
if record.attribute do
  handle_present_attribute(...)
else
  # unselected attributes would have ended up in this branch
  handle_not_present_attribute(...)
end
```

Now, if it is possible for that attribute to have not been selected, you'll want to do something like this instead:

```elixir
case record.attribute do
  %Ash.NotLoaded{} ->
    handle_not_selected(...)
  nil ->
    handle_not_present_attribute(...)
  value ->
    handle_present_attribute(...)
end
```

## Upgrading to 2.0

All deprecations will be finalized in version 2.1.

### Ash.Flow

While still more experimental than the rest of the framework, Ash.Flow is no longer feature-gated behind a configuration flag. It has been changed only slightly, and now returns an `Ash.Flow.Result` in all cases.

### New DSL tooling

The DSL tooling has been moved out of the Ash name space and into a more generalized tool called `Spark`. If you have written your own extensions, you will need
to refer to those modules. They are all the same, but they have different names. You will get compiler errors/warnings on the modules you need to change, for example: `Ash.Dsl` -> `Spark.Dsl` and `Ash.Dsl.Transformer` -> `Spark.Dsl.Transformer`. One exception, `Ash.Error.Dsl.DslError`, has been changed to `Spark.Error.DslError`.

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

The following functions have been deprecated, and will be removed in 2.1

- `Ash.Changeset.replace_relationship/4` - use `manage_relationship/4` instead. 
- `Ash.Changeset.append_to_relationship/4` - use `manage_relationship/4` instead. 
- `Ash.Changeset.remove_from_relationship/4` - use `manage_relationship/4` instead. 

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