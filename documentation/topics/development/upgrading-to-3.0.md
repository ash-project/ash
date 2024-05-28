# Upgrade

## Other Packages

Other packages have had a major version bump in addition to Ash core. While _all packages_ have been changed to refer to `domain` instead of `api`, they did not receive a major version bump because there were no special breaking changes to account for when using that package. You will also need to factor in the following upgrade guides, if you use those packages.

- [AshPostgres](https://hexdocs.pm/ash_postgres/upgrading-to-2-0.html)
- [AshJsonApi](https://hexdocs.pm/ash_json_api/upgrade.html)
- [AshGraphql](https://hexdocs.pm/ash_graphql/upgrade.html)

## Upgrading to 3.0

This section contains each breaking change, and the steps required to address it in your application

### Dependency Changes

#### Ash.Flow

If you use `Ash.Flow`, include `{:ash_flow, "~> 0.1.0"}` in your application.

#### Picosat Elixir

In 2.0, Ash had a dependency on `picosat_elixir`. In 3.0, this is an optional dependency, to help folks handle certain compatibility issues. To upgrade, add `{:picosat_elixir, "~> 0.2"}` to your `mix.exs`.

### Ash.Api is now Ash.Domain

The previous name was often confusing as this is an overloaded term for many. To that end, `Ash.Api` has been renamed to `Ash.Domain`, which better fits our usage and concepts.

#### What you'll need to change

To make this change you will need to do two things:

1. replace `Ash.Api` with `Ash.Domain` in your application
2. replace places where an `:api` option is passed to a function with the `:domain` option. For example, `AshPhoenix.Form.for_create(..., api: MyApp.SomeApi)` should now be `AshPhoenix.Form.for_create(..., domain: MyApp.SomeDomain)`
3. Update your application config to define `ash_domains` instead of `ash_apis`, eg. `config :my_app, ash_domains: [MyApp.MyDomain]`

### DSL Changes

- `code_interface.define_for` is now `code_interface.domain`. Additionally, it is set automatically if the `domain` option is specified on `use Ash.Resource`.

- `domain.execution.timeout` used to default to 30 seconds, but now it defaults to `:infinity`. This is because a timeout requires copying memory across process boundaries, and is an unnecessary expense a _vast_ majority of the time. We recommend putting timeouts on specific actions that may need them.

- `actions.create.reject`, `actions.update.reject` and `actions.destroy.reject` have been removed. Blacklisting inputs makes it too easy to make mistakes. Instead, specify an explicit `accept` list.

- `relationships.belongs_to.attribute_writable?` no longer makes the underlying attribute both public and writable. It defaults to the value of `writable?` on the relationship (which itself defaults to `true`), and only controls the generated attributes `writable? true` property. So now, by default, it will be `true`, which is safe when coupled with changes to the `default_accept`, discussed below. Generally, this means you should be safe to remove any occurrences of `attribute_writable? true`.

- `relationships.belongs_to.attribute_public?` has been added, which controls the underlying attribute's `public?` value. This, similar to `attribute_writable?` defaults to the `public?` attribute of the relationship.

- `resource.simple_notifiers` has been removed, in favor of specifying non-DSL notifiers in the `simple_notifiers` option to `use Ash.Resource`.

- `resource.actions.read.filter` can now be specified multiple times. Multiple filters will be combined with `and`.

### `Ash.Registry` has been removed

`Ash.Registry` is no longer needed. Place each resource in the domain instead.

```elixir
resources do
  resource Resource1
  resource Resource2
end
```

### Expression changes

When calling a calculation with arguments, this is done via passing a keyword list to the calculation, for example: `full_name(separator: "")`. In 2.0, keyword lists were not evaluated as part of the expression in the same way as other values, meaning two things:

1. You did not have to pin usage of template functions, i.e `full_name(separator: arg(:separator))`. Now, you will need to do so: `full_name(separator: ^arg(:separator))`

2. You had to use `expr` to pass an expression to a calculation argument (this only works if `allow_expr? true` is configured on the calculation argument). For example: `full_name(separator: expr(sep_1 <> sep_2))` would now be `full_name(separator: sep_1 <> sep_2)`

If you do not have any expression calculations that accept arguments, you likely need to do nothing. To make these changes, you will need to look at each place you build an expression that you may be calling a calculation w/ arguments, i.e `Ash.Query.filter`, and the `expression` callback in `Ash.Calculation`, and see if they must be modified as described above.

### Module/function changes

#### Ash.Policy.FilterCheck and Ash.Policy.FilterCheckWithContext are combined

`Ash.Policy.FilterCheck` and `Ash.Policy.FilterCheckWithContext` have been combined into `Ash.Policy.FilterCheck`. If you have any usages of `FilterCheckWithContext`, you'll need to change that to `FilterCheck`. If you have usages of `FilterCheck`, you will need to add the `context` arguments to the callbacks. Compiler warnings will show you what callbacks mismatch.

#### Ash.Filter

`Ash.Filter.parse/5` is now `Ash.Filter.parse/3`. `Ash.Filter.parse_input/5` is now `Ash.Filter.parse_input/2` The third and fourth optional arguments are unnecessary and were previously ignored, and the fifth argument is not necessary for `parse_input`.

`Ash.Filter.used_aggregates/3` no longer accepts `:all` as a relationship path, instead using `:*`. Its very unlikely that this is used in your application.

#### Ash.Filter.TemplateHelpers

Tools for templating expressions were previously in `Ash.Filter.TemplateHelpers`. This often led to confusion because it was a hard to remember module name, and didn't really make sense to be separate from the rest of our utilities. Now, all the functions/macros you need for expressions are in `Ash.Expr`. This means that in any given file where you want to work with expressions, you only need to do `import Ash.Expr`. Additionally, this `import Ash.Expr` has been added to changes, preparations, validations and calculations automatically.

#### Ash.CiString

- `Ash.CiString.new(nil)` now returns `nil` instead of `%Ash.CiString{value: nil}`

#### Ash.Resource.Validation

`validate/2` is now `validate/3`, with the third argument being the context of the validation.

#### Ash.Query.Calculation

The function signature of `Ash.Query.Calculation.new` has been changed. We use an options list over optional arguments, and now require constraints to be provided. You will need to adjust your calls to this function.

#### Ash.Calculation

This module has been renamed to `Ash.Resource.Calculation`. You will need to rename your references to it.

#### Ash.Query

Ash.Query.to_query has been removed. Use `Ash.Query.new` instead.

Ash.Query.expr has been removed. Use `Ash.Expr.expr` instead.

#### Aggregates

`first` and `list` aggregates have a new option called `include_nil?`, which _defaults to false_. You may need to add `include_nil?: true` to your resource aggregates if you wish to retain the old behavior.

##### New format for sorting on calculations with arguments

The format for sorting on calculations that take input has been swapped. Previously, you would use `sort(calculation: {:desc, %{arg: :value}})`, but for the sake of consistency, you now use `sort(calculation: {%{arg: :value}, :desc})`.

#### Ash.Changeset

`Ash.Changeset.new/2` has been removed. `Ash.Changeset.new/1` is still available for creating a new changeset, but attributes and arguments should, with few exceptions, be passed to the relevant `Ash.Changeset.for_<action_type>` functions, _not_ to `Ash.Changeset.new/2`. Removing the second argument helps clarify the purpose of `Ash.Changeset.new/1`.

`Ash.Changeset.after_transaction/2` can no longer be called from within other lifecycle hooks. We need to know whether or not an after action hook, before we start processing any hooks.

`Ash.Changeset.manage_relationship/4` no longer uses `:all` to signal that all changes will be sent to the join relationship. Instead, use `:*`.

`Ash.Changeset.filter` now accepts expressions. The value of the filter is no longer a simple equality map, but rather a regular Ash expression. We add to it on successive calls to `Ash.Changeset.filter`. Additionally, this value is stored in `changeset.filter` instead of `changeset.filters`.

#### Ash.Policy.FilterCheckWithContext

`Ash.Policy.FilterCheck` and `Ash.Policy.FilterCheckWithContext` have been combined. The name is `Ash.Policy.FilterCheck`, but the callbacks take the extra arguments present in `Ash.Policy.FilterCheckWithContext`.

#### Builtin Changes & Preparations

The functions provided to `after_action/1`, `after_transaction/1`, `before_transaction/1` and `before_action/1` must all now take an additional argument, which is the change context.

For example,

```elixir
change after_action(fn changeset, result -> ... end)
```

is now

```elixir
change after_action(fn changeset, result, context -> ... end)
```

This is true for both preparations and changes.

#### Expressions

Previously, in expressions, you could say `expr(ref(^some_atom))`. This is a tool for building dynamic references, but it was an exception to the standard pattern of prefixing "external" things in an expression, i.e `arg` with `^`. Now, you must do the same with `ref/1` and `ref/2`. You will need to search for `ref(` in your application, and ensure that if it is inside of an expression you have prefixed it with `^`. The original example becomes: `expr(^ref(some_atom))`.

#### Exception changes

##### Ash exceptions have been simplified and are now backed by `Splode`

Usage of `def_ash_error/2` will show you what to change in its warnings.

Instead of combining `def_ash_error` with `defimpl Ash.ErrorKind`, you create a custom error like so:

```elixir
defmodule MyCustomError do
  use Splode.Error, class: :invalid, fields: [:foo, :bar]

  def message(error) do
    "Message: #{error.foo} - #{error.bar}"
  end
end
```

##### Ash exception changes

When sorting or filtering, if a field is not found, an `Ash.Query.Error.NoSuchField` is used, where it would have previously been an `Ash.Query.Error.NoSuchAttribute`. This was wrong as sometimes the field reference was not an attribute. Places that would previously return `Ash.Query.Error.NoSuchAttributeOrRelationship` now return `Ash.Query.Error.NoSuchField` as well.

Additionally, the following exceptions have had keys remapped:

`NoSuchAttribute`: `name` -> `attribute`
`NoSuchRelationship`: `name` -> `relationship`
`NoSuchFunction`: `name` -> `function`
`NoSuchOperator`: `name` -> `operator`

---

## Significant Changes

### `Ash.set_*` functions have been removed

In 2.0, a set of features allowed storing the actor, tenant and context in the process dictionary. There were fundamental issues with this pattern that manifested in subtle bugs. We suggest making this change _before_ you upgrade, as this change can be made and verified without upgrading to 3.0.

#### What you'll need to change

You need to manually thread through your tenant, actor, and context values wherever you were using `Ash.set_*`. For example:

```elixir
Ash.set_actor(current_user)
Ash.set_tenant(current_tenant)

Ash.Changeset.for_create!(..)
Ash.Query.for_read(..)
```

would become

```elixir
Ash.Changeset.for_create!(.., tenant: current_tenant, actor: current_user)
Ash.Query.for_read(.., tenant: current_tenant, actor: current_user)
```

### The `Domain` of a resource must now be known when constructing a changeset, query or action input

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

While it is possible for resources to be used with multiple domains, it almost never happens in practice. Any resources that are only used from a single domain only (_not_ including embedded resources) should be modified to have a `domain` option specified in their call to `use Ash.Resource`. For example:

```elixir
use Ash.Resource,
  domain: MyApp.MyDomain
```

###### Using `Ash.*` to interact with your resources

Calling functions on the domain has been deprecated. You must now use the functions defined in the `Ash` module to interact with your resources. They are the same as what was previously available in your domain module. For example:

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

For these, you will need to include the `domain` option when you construct a changeset.

For example:

```elixir
MyResource
|> Ash.Changeset.for_create(:create, input, domain: MyApp.MyDomain)
```

---

### Actions no longer default to accepting all public writable attributes

For more context, see the original discussion: https://github.com/ash-project/ash/issues/512

In 2.0, all public, writable attributes were accepted by each action by default. This made it very easy to accidentally expose writing to an attribute in an action where that was not the intent. Additionally, new attributes added were automatically writable across a wide array of actions, which was error prone for the same reason.

In 2.0, as well as 3.0, there is an option called `default_accept`, which modifies all actions that do not have an `accept` list. In 2.0, the default value for `default_accept` was "all public, writable attributes". In 3.0, the default value for `default_accept` is `[]`. This encourages a pattern of explicitly listing inputs to actions, and is safer and less error prone.

#### What you'll need to change

For those who want to upgrade, you would use the new `:*` option to `default_accept` (also usable in an action's `accept` option) to accept all public attributes. Go to each resource and, inside the actions block, add:

```elixir
actions do
  default_accept :*
  ...
end
```

Then mark the attributes and relationships you want to accept as `public?: true` (see [this section](#private-true-public-true) for more information on this change).

For those who want to be more explicit, or after your upgrade has complete if you wish to refactor existing resources and actions, the general best path forward is to copy the `default_accept` into each action (or put it in a module attribute and reference it) as the `accept` option. This way when a new action is added, it does not "inherit" some list of accepted attributes.

> ### :\* private attributes can now be accepted {: .info}
>
> In 2.0, accepting a private attribute as a change required adding an argument with the same name, and using `change set_attribute(...)`. Now that we require explicit accept lists, you can place private attribtues in that list, which will allow them to be written to (but not read back).

> ### :\* includes belongs_to attributes! {: .warning}
>
> The change to explicit accepts also included a change that defaults belongs_to attributes to `writable?: true` and `public?: false`. You may want to add `attribute_writable?: false` to your belongs_to relationships if you are adding `default_accept :*` and don't currently have `attribute_writable?: true` on them currently.

---

### Default actions `:create` and `:update` can now have an accept list

For example:

```elixir
defaults [:read, :destroy, create: :*, update: :*]
```

### Default read actions are now paginatable

In 2.0, if you have `:read` in your default actions list, it would generate an action like this:

```elixir
read :read do
  primary? true
end
```

Now, it generates an action like this:

```elixir
read :read do
  primary? true
  pagination [keyset?: true, offset?: true, countable: true, required?: false]
end
```

---

### What you will need to change

For most cases, this won't affect you. However, if you are using `AshGraphql`, and have any queries connected to a default `:read` action, it will default to making those queries paginatable with keyset pagination. To keep the old behavior, you will need to add `paginate_with nil` to the query, for example:

```elixir
graphql do
  queries do
    list :list_things, :read, paginate_with: nil
  end
end
```

---

### Before action and before transaction hooks order has been reversed

In Ash 2.0, `before_action` and `before_transaction` hooks that were added to a changeset were prepended to the list of hooks by default. These hooks were then run in order. What this meant is that, given an action like the following:

```elixir
create :foo do
  change before_action(fn changeset, _context ->
    IO.puts("first")
    changeset
  end)

  change before_action(fn changeset, _context ->
    IO.puts("second")
    changeset
  end)
end
```

You would see `second` printed _before_ `first`.

#### What you'll need to change

In many cases, this won't matter to you. However, if you have a situation where the order of your before action/transaction hooks matters, you can do one of two things:

1. reorder the changes that add those before action/transaction hooks
2. use the `:prepend` option to `Ash.Changeset.before_action/2` and `Ash.Changeset.before_transaction/2` to explicitly prepend the hook to the list of hooks

---

### Context in changes, preparations, validations, calculations are now structs

To help make it clear what keys are available in the context provided to callbacks on these modules, they have been adjusted to provide a _struct_ instead of a `map`. This helps avoid potential ambiguity, and
acts as documentation.

#### What you'll need to change

If you are using something like `Keyword.new(context)` to generate options to pass into an action, change that to `Ash.Context.to_opts(context)`.

---

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

---

### `private?: true` -> `public?: true`

There is no longer a `private?` option for attributes, relationships, calculations and aggregates. Instead of attributes defaulting to `private?: false`, they now default to `public?: false`. It was too easy to add an attribute and not realize that you had exposed it over your api.

#### What you'll need to change

If you are using api extensions (i.e `AshGraphql` and `AshJsonApi`), you will need to go to your resources and "invert" the definitions. i.e _remove_ `private?: true` and _add_ `public?: true` to _every other_ attribute, _relationship_ and _calculation_. Don't forget the relationships and calculations!

> ### Embedded resources too! {: .WARNING}
>
> The above includes embedded resources as well! Don't forget to make sure that all fields on your embedded resources are also marked as `public?: true` (if applicable). The goal here is to have a clear visual indicator of what in your application can be shown publically.

---

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

---

### Calculation loads do not select all related fields by default

In 2.0 relationship loads from the `load/3` callback in a calculation will select _all_ fields of that relationship and make them available to the calculation.

For example, the following calculation `load/3` callback expresses a dependency on _all_ fields of the relationship `:relationship`.

```elixir
def load(_, _, _) do
  [:relationship]
end
```

#### What you'll need to change

##### Refactor

In 3.0, relationship dependencies alone will only make the related primary keys available. You now need to select explicit fields that you want to use in your calculation, for example:

```elixir
def load(_, _, _) do
  [relationship: [:field1, :field2]]
end
```

#### Keep the old behavior

Each calculation can still opt into the old behavior by adding the callback `strict_loads/0` and returning `false`.

```elixir
def load(_, _, _) do
  [:relationship]
end

def strict_loads, do: false
```

---

### Calculations do not have a `select/3` callback any more

In 2.0 calculations had a `select/3` callback, but `load/3` is now a superset of `select/3` and so the former is no longer needed.

#### What you'll need to change

If you have a `select/3` callback in your calculations, you will need to remove the `select/3` callback. You must then add those fields to the `load/3` callback.

For example:

```elixir
def select(_, _, _), do: [:some_attribute]
def load(_, _, _), do: [:some_calculation, some_relationship: [:some_field1, :some_field2]]
```

can now be written more simply as:

```elixir
def load(_, _, _), do: [:some_attribute, :some_calculation, some_relationship: [:some_field1, :some_field2]]
```

---

### Embedded resources no longer have `autogenerated_id`

A private primary key called `autogenerated_id` was added to embedded resources if no primary key was added manually.

This should have no real effect on your application, except for the fact that your embedded attributes will have `autogenerated_id` in the database which won't be reflected by an attribute any more. If it is updated, then the `autogenerated_id` field will go away.

This is listed as a breaking change in case someone is depending on this feature, but that should be very uncommon/unlikely.

---

### PubSub notifier no longer publishes events for previous values by default

Previously, the Ash notifier would publish a message containing both the old _and_ new values for changing attributes. Typically, we use
things like IDs in notification topics, that do not change, so for most this will not have an impact.

If you wish to send a notification for the old value and the new value, then an action cannot be done atomically. Bulk actions must update each record in turn, and atomic updates can't be leveraged.

If you're comfortable with the performance implications, you can restore the previous behavior by addding `previous_values?: true` to your publications in your pub_sub notifier

```elixir
publish :update, ["user:updated", :email], previous_values?: true
```

---

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

---

### `Domain.authorization.authorize` now defaults to `:by_default`

Previously, the default was `:when_requested`. This meant that, unless you said `actor: some_actor` or `authorize?: true`, authorization was skipped. This has the obvious drawback of making it easy to accidentally bypass authorization unintentionally. In 3.0, this now defaults to `:by_default`.

#### What you'll need to change

##### Keep old behavior

To avoid making a significant refactor, and to keep your current behavior, you can go to your domain and set the configuration below. Otherwise skip to the refactor steps below. We advise that you take this route to start, but we _highly suggest_ that you change your domains to `authorize :by_default` in the future. `authorize :when_requested` will not be deprecated, so there is no time constraint.

```elixir
authorization do
  authorize :when_requested
end
```

##### Refactor

For each domain that has the old configuration, after setting it to the new config, you'll need to revisit each call to that domain that doesn't set an actor or the `authorize?` option, and add `authorize?: false`.

This may be a good time to do the refactor from `YourDomain.func` to `Ash.func`, if you want to. See the section about domains being required when building changesets.

---

### `require_atomic?` defaults to `true`

On `:update` actions, and `:destroy` actions, they now default to `require_atomic? true`. This means that the following things will cause errors when attempting to run the action:

1. changes or validations exist that do not have the `atomic` callback. _This includes anonymous function changes/validations_.
2. attributes are being changed that do not support atomic updates. This most notably includes (for now) embedded resources.
3. the action has a manual implementation
4. the action has applicable notifiers that require the original data.

Updates and destroys that can be made fully atomic are always safe to do concurrently, and as such we now require that actions meet this criteria, or that it is explicitly stated that they do not have to. See the [update actions guide](/documentation/topics/actions/update-actions.md#fully-atomic-updates) for more.

#### Upgrade config

You can set the following configuration, which _will be removed in Ash 3.1_. This configuration will retain the 2.0 default behavior of `require_atomic?` defaulting to `false`. You can then safely do the rest of the upgrade. Then, you can perform this one change after confirming that your system works as expected.

```elixir
config :ash, :require_atomic_by_default?, false
```

#### What you'll need to change

The vast majority of cases will be caught by warnings emitted at compile time.

##### Anonymous function changes

Anonymous function changes can never be made atomic, because we don't know what they contain. You will either need to transfer it to a module change and then follow the next section, or set `require_atomic? false`

##### Module changes

If you have a module change, you can make it atomic by defining the `atomic/3` callback. This callback can replace the `change/3` callback, but it is very important to keep in mind that _later changes_ will no longer have access to the value. For example, if you have

```elixir
def change(changeset, _, _) do
  # this is not concurrency safe
  Ash.Changeset.change_attribute(changeset, :value, changeset.data.value + 1)
end
```

If you have a subsequent change that does something like `Ash.Changeset.get_attribute(changeset, :value)` it will get the new value (i.e old value + 1). With atomics, `Ash.Changeset.get_attribute(changeset, :value)` would return the _old value_. This is because atomics are scheduling an update that happens when call the data layer. For example:

```elixir
def atomic(changeset, _, _) do
  {:atomic, %{value: expr(value + 1)}}
end
```

This should not typically matter unless you have complex actions w/ multiple changes where subsequent changes need to know the results of previous steps. In those cases, if you can't make them all atomic, then its best just not to worry about it and set `require_atomic? false`

##### Non-atomic castable types

If you are using `change atomic_update/2` or `Ash.Changeset.atomic_update/2` or `Ash.Changeset.atomic_update/3`, and the type does not support atomic updates, you will get an error unless you do one of the following:

1. for `change atomic_update/2` add the `cast_atomic?: false` option.
2. for `Ash.Changeset.atomic_update`, pass the value as `{:atomic, expr}`, i.e `Ash.Changeset.atomic_update(changeset, :value, {:atomic, expr(value + 1)})`

For builtin types, the above applies to `:union`, `:map`, `:keyword`, embedded types. It also applies to `:string`, but only if the `match?` constraint is present.

---

### `Ash.Error.Invalid.NoSuchInput` errors on unknown action inputs

In 2.0, inputs to actions that don't match an accepted attribute or argument were silently ignored. This made it very easy to make certain kinds of mistakes, like assuming that an input is being used by an action when it actually is not. Now, unknown action inputs will cause an `Ash.Error.Invalid.NoSuchInput`.

#### What you'll need to change

If you have action calls that are erroneously passing in extra values, you will need to do remove them.

A logic error was fixed in this behavior for embedded resources. If you are using embedded resources in `{:array, _}` types, and are relying on including the primary key of that embedded resource to match records up for updating/destroy behavior, you will need to make sure that you do one of the following

1. add the `writable?: true` flag to the uuid of the embedded resource (probably what you want)
2. modify the actions to accept an `id` argument and set the argument to the provided value

---

### `%Ash.NotLoaded{}` for attributes

In 2.0, attributes that were not selected were replaced with `nil` values. This could lead to confusion when dealing with records that didn't have all attributes selected. If you passed these records to a function it might see that an attribute is `nil` when actually it just wasn't selected. To find out if it was selected, you could look into `record.__metadata__.selected`, but you'd have to know to do that. To alleviate these issues, attributes that are not selected are now filled in with `%Ash.NotLoaded{}`, just like calculations and aggregates.

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

---

### Calculations do not reuse values by default

When loading data in 2.0 the option `reselect_all?` defaulted to `false`. What this would mean is that existing values for attributes would be reused, instead of visiting the data layer, by default. This can be an extremely valuable piece of behavior, but _defaulting_ to it often means accidentally using data as a cache that you did not intent to use as a cache. Take the following example:

```elixir
user = %User{first_name: "fred", last_name: "weasley"}

Ash.update!(user, first_name: "george")

user |> Ash.load!(:full_name)
# in 2.0 -> fred weasley
# in 3.0 -> george weasley
```

To opt into the old behavior, which we recommend doing on a case-by-case basis, you can pass `reuse_values?: true`. For example:

```elixir
user |> Ash.load!(:full_name, reuse_values?: true)
```

---

### Resources are not interchangeable with `Ash.Type` anymore

In 2.0 it was possible to pass an Ash resource in all places where some instance of `Ash.Type` was supported. In 3.0 resources (except for embedded resources) don't implement the `Ash.Type` behaviour anymore.

#### What you'll need to change

If you were using a resource in one of the places that accept an `Ash.Type` (arguments, calculation return values or fields of a union) you have to refactor your code to use the `:struct` type together with an `instance_of` constraint

```elixir
calculation :random_post, :struct do
  constraints instance_of: Post
  calculate Calculations.RandomPost
end
```
