# Upgrade

## Upgrading to 3.0

This section contains each breaking change, and the steps required to address it in your application

### DSL Changes

* `code_interface.define_for` is now `code_interface.domain`. Additionally, it is set automatically if the `domain` option is specified on `use Ash.Resource`.

### `Ash.Registry` has been removed

`Ash.Registry` is no longer needed. Place each resource in the domain instead.

```elixir
resources do
  resource Resource1
  resource Resource2
end
```

### Module/function changes

#### Ash.Filter

Ash.Filter.parse/5 is now `Ash.Filter.parse/3`. Ash.Filter.parse_input/5 is now `Ash.Filter.parse_input/2` The third and fourth optional arguments are unnecessary and were previously ignored, and the fifth argument is not necessary for `parse_input`.

#### Ash.Resource.Validation

`validate/2` is now `validate/3`, with the third argument being the context of the validation.

#### Ash.Query.Calculation

The function signature of `Ash.Query.Calculation.new` has been changed.  We use an options list over optional arguments, and now require constraints to be provided. You will need to adjust your calls to this function.

#### Ash.Calculation

This module has been renamed to `Ash.Resource.Calculation`. You will need to rename your references to it.

#### Builtin Changes

The functions provided to `after_action/1`, `after_transaction/1`, `before_transaction/1` and `before_action/1` must all now take an additional argument, which is the change context.

For example,

```elixir
change after_action(fn changeset, result -> ... end)
```

is now

```elixir
change after_action(fn changeset, result, context -> ... end)
```

### Ash.Api is now Ash.Domain

The previous name was often confusing as this is an overloaded term for many. To that end, `Ash.Api` has been renamed to `Ash.Domain`, which better fits our usage and concepts. 


#### What you'll need to change

To make this change you will need to do two things:

1. replace `Ash.Api` with `Ash.Domain` in your application
2. replace places where an `:api` option is passed to a function with the `:domain` option. For example, `AshPhoenix.Form.for_create(..., api: MyApp.SomeApi)` should now be `AshPhoenix.Form.for_create(..., domain: MyApp.SomeDomain)`

### Context in changes, preparations, validations, calculations are now structs

To help make it clear what keys are available in the context provided to callbacks on these modules, they have been adjusted to provide a *struct* instead of a `map`. This helps avoid potential ambiguity, and
acts as documentation.

#### What you'll need to change

If you are using something like `Keyword.new(context)` to generate options to pass into an action, change that to `Ash.context_to_opts(context)`.

### Calculation arguments are now in `context.arguments`

Per the above change, we have specified the values available in the context of a calculation, with `Ash.Resource.Calculation.Context`. In Ash 2.0, context was merged with arguments, which was problematic in various ways. Now, arguments are in `context.arguments`. 

#### What you'll need to change

You will need to update your module-backed calculations to account for this. 

```elixir
def calculate(records, _opts, context) do
  Enum.map(records, fn record -> 
    record.first_name <> context.delimiter <> record.last_name
  end)
end
```

would need to be adjusted to access arguments in the context:

```elixir
def calculate(records, _opts, %{arguments: arguments}) do
  Enum.map(records, fn record -> 
    record.first_name <> arguments.delimiter <> record.last_name
  end)
end
```

### Anonymous calculations now operate on a list, just like module calculations

Previously, anonymous function calculations were special cased to operate on a single record. For consistency, these anonymous functions now take the list of records.

#### What you'll need to change

Update any anonymous function calculations to take and return a list, for example:

```elixir
calculate :full_name, :string, fn record, _context -> 
  record.first_name <> " " <> record.last_name
end
```

would become

```elixir
calculate :full_name, :string, fn records, _context -> 
  # note, you can also return `{:ok, list}` or `{:error, error}`
  Enum.map(records, fn record -> 
    record.first_name <> " " <> record.last_name
  end)
end
```

### PubSub notifier no longer publishes events for previous values by default

Previously, the Ash notifier would publish a message containing both the old *and* new values for changing attributes. Typically, we use
things like IDs in notification topics, that do not change, so for most this will not have an impact.

If you wish to send a notification for the old value and the new value, then an action cannot be done atomically. Bulk actions must update each record in turn, and atomic updates can't be leveraged.

If you're comfortable with the performance implications, you can restore the previous behavior by addding `previous_values?: true` to your publications in your pub_sub notifier

```elixir
publish :update, ["user:updated", :email], previous_values?: true
```

### Custom checks and notifiers will not have access to the original data by default

In your notifiers and policy checks, when you get a changeset you currently have access to the `data` field,
which is the original record prior to being updated or destroyed. However, this is not compatible with atomic/bulk
updates/destroys, where we may be given a query and told to destroy it. In those cases, `changeset.data` will be
`%Ash.Changeset.OriginalDataNotAvailable{}`. When you write a custom check or a custom notifier, if you need access to the original data, you must add the following function:

```elixir
# in custom checks
def requires_original_data?(_authorizer, _opts), do: true

# in notifiers
def requires_original_data?(_resource, _action), do: true
```

Keep in mind, this will prevent the usage of these checks/notifiers with atomic actions.

### the `Domain` of a resource must now be known when constructing a changeset, query or action input

In order to honor rules on the `Domain` module about authorization and timeouts, we have to know the `Domain` when building the changeset.

#### What you'll need to change

##### Embedded Resources

The domain for the calls to embedded resources is gotten from the parent changeset. No need to change them at all. a `domain` constraint has been added in case you wish to make a given embedded resource use a specific domain always.

For example:

```elixir
attribute :bio, MyApp.Bio do
  constraints domain: MyApp.SomeDomain
end
```

##### Single Domain resources

While it is possible for resources to be used with multiple domains, it almost never happens in practice. Any resources that are only used from a single domain only (*not* including embedded resources) should be modified to have a `domain` option specified in their call to `use Ash.Resource`. For example:

```elixir
use Ash.Resource,
  domain: MyApp.MyDomain
```

###### Using `Ash.*` to interact with your resources

For resources that have a static domain configured, you can now use functions in `Ash` to interact with your resources. They are the same as what is available in your domain module. For example:

```elixir
MyDomain1.create!(changeset)
MyDomain2.read!(query)
MyDomain3.calculate!(...)
```

can now be written as

```elixir
Ash.create!(changeset)
Ash.read!(query)
Ash.calculate!(query)
```

This makes refactoring resources easier, as you no longer need to change the call site, it remains the same regardless of what Domain a resource is in.

##### Multi Domain resources

For these, you will need to include the `domain` option every time you construct a changeset.

For example:

```elixir
MyResource
|> Ash.Changeset.for_create(:create, input, domain: MyApp.MyDomain)
```

### `Domain.authorization.authorize` now defaults to `:by_default`

Previously, the default was `:when_requested`. This meant that, unless you said `actor: some_actor` or `authorize?: true`, authorization was skipped. This has the obvious drawback of making it easy to accidentally bypass authorization unintentionally. In 3.0, this now defaults to `:by_default`.

#### What you'll need to change

##### Keep old behavior

To avoid making a significant refactor, and to keep your current behavior, you can go to your domain and set the configuration below. Otherwise skip to the refactor steps below. We advise that you take this route to start, but we *highly suggest* that you change your domains to `authorize :by_default` in the future. `authorize :when_requested` will not be deprecated, so there is no time constraint.

```elixir
authorization do
  authorize :when_requested
end
```

##### Refactor

For each domain that has the old configuration, after setting it to the new config, you'll need to revisit each call to that domain that doesn't set an actor or the `authorize?` option, and add `authorize?: false`.

This may be a good time to do the refactor from `YourDomain.func` to `Ash.func`, if you want to. See the section about domains being required when building changesets.

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