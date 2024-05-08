## [v3.0.0](https://github.com/ash-project/ash/compare/v3.0.0...2.0)

## 3.0

We are starting the changelog fresh. See `documentation/2.0-CHANGELOG.md` in GitHub for the old changelogs.

### Breaking Changes:

For a guide on adjusting to these breaking changes, see the [upgrade guide](/documentation/topics/development/upgrading-to-3.0.md)

- [Ash.Api] has been renamed to `Ash.Domain`, and references to the concept have been renamed as well, i.e in options and in the DSL
- [Ash] we now call functions on this, isntead of the domain. i.e `Ash.create` and `Ash.read`. The generated functions are now marked as deprecated
- [Ash] remove process context functionality. You can no longer store the actor/tenant in the context with `Ash.set_actor` and so on
- [private?] deprecate `private?: false` in favor of the more explicit `public?: true`
- [default_accept] default `default_accept` is now `[]`
- [action lifecycle] after transaction hooks cannot be added from inside of other lifecycle hooks
- [Ash.NotLoaded] use `%Ash.NotLoaded{}` for unselected values, instead of `nil`
- [require_atomic?] now defaults to `true`, requiring opt-out of atomic behavior
- [authorization] default `api.authorization.authorize` to `:by_default`
- [Ash.Registry] has been removed
- [actions] `domain` must always be known when constructing changesets
- [Ash.Notifier] `requires_original_data?/2` callback defaults to `false`
- [Ash.Notifier.PubSub] default to `previous_values?: false`, allowing notifications to be sent for atomic updates
- [unknown inputs] all action invocations now use `UnknownInput` errors when given an input they don't accept
- [policies] `requires_original_data?/2` callback on checks defaults to `false`
- [Ash.Calculation] has been renamed to `Ash.Resource.Calculation`
- [Ash.Resource.Calculation] "strict mode" has been added and defaults to `true`. This causes only explicitly requested fields from relationships to be loaded
- [Ash.Query.Calculation] positional arguments are now an options list
- [calculations] anonymous function calculations in a resource now take lists and return lists, instead of a single record (like standard calculations do)
- [context] The context argument passed to many different callbacks is now a struct, tailored to that specific context. For example, in a calculation you will receive an `Ash.Resource.Calculation.Context`
- [after_action/before_action] These builtin changes now accept a 3rd context argument
- [picosat_elixir] is now optional (`simple_sat` is now an alternative)
- [Ash.Changeset] `Ash.Changeset.new!` has been removed
- [Ash.Changeset] `Ash.Changeset.new/2` has been removed (`Ash.Changeset.new/1` is still available)
- [Ash.Changeset] `changeset.filters` is now `changeset.filter`
- [Ash.Changeset] reverse order of before action & before transaction hooks. They now run in the action they are added. They used to run in reverse order.
- [Ash.CiString] `Ash.CiString.new/1` returns `nil` on `nil` input
- [belongs_to.attribute_writable?] add `attribute_public?` for controlling publicity, and default `attribute_writable?` to `true`.
- [Ash.Filter.TemplateHelpers] removed, all functions needed for expressions are now defined in `Ash.Expr`
- [Ash.Expr] keyword lists are no longer special cased in ash expressions, and requiring pinning like any other value.
- [Ash.Resource] default read actions are now paginatable with keyset and offset pagination (but pagination is not required)
- [Ash.Resource] default actions require explicit accept lists (or will use `default_accept`). i.e `defaults [:read, create: [:first_name, :last_name]]`
- [Ash.Resource] `simple_notifiers` is now an option to `use Ash.Resource`, instead of being in the DSL at `resource.simple_notifiers`
- [Ash.Flow] has been removed and put in its own package `ash_flow`. It is being deprecated in favor of `Reactor`
- [Ash.Error] the implementation has been extracted out to `Splode`. Defining new `Ash.Error`s is now done by defining a new `Splode.Error`
- [Ash.Query] swap position of sort order and arguments in calculation sorting, i.e instead of `calculation: {:asc, %{...args}}` it is now `calculation: {%{...args}, :asc}`
- [Ash.Resource.Aggregate] add `include_nil?` aggregate option, and default it to `false` (so `list` and `first` aggregates do not consider `nil` values by default)
- [Ash.Policy.FilterCheck] now accepts `context` arguments, like `Ash.Policy.FilterCheckWithContext`
- [Ash.Policy.FilterCheckWithContext] has been removed, use `Ash.Policy.FilterCheck`

### Features:

- [Ash.Type] add new remove_nil_items? array type constraint (#1116)
- [Ash.Query] Paginatable relationships (#1050)
- [Ash.DataLayer] new `calculate/3` callback that allows for data layers to compute the result of expressions outside the context of a query. Used to power `Ash.calculate/3`.
- [validations] new builtin validations, `attributes_present/2` and `attributes_absent/2`
- [multitenancy] configurable multitenancy behaviour on read actions (#1030)
- [Ash.Reactor] Add new `change` step type which can be used to modify changesets.
- [Ash.Changeset] add `Ash.Changeset.update_change/2` function and builtin change (#976)
- [Ash.Domain] code interfaces can now be defined on the domain
- [Ash.Domain] policies can now be defined on the domain, and will run before resource policies
- [Ash.ToTenant] add `Ash.ToTenant`, allowing for passing arbitrary values as tenants
- [Ash] add `Ash.read_first` (like `Ash.read_one`, but applies a limit automatically)
- [Ash] support a second optional `input` option for `create`, `update` and `destroy`, allowing for things like `Ash.create!(Post, %{text: "text"}, opts)`
- [sensitive?] support `sensitive?` option in query aggregate/calculation (#963)
- [Ash.Resource] support `require_reference?: false` on code interfaces, for when an update or destroy action uniquely identifies a record (or for bulk update/destroy)
- [Ash.Resource] notifiers can now be specified for specific actions, using the `notifiers` option
- [mix ash.rollback] delegates to extensions to trigger their rollback tasks
- [Ash.Query] add `Ash.Query.apply_to/3`, to "apply" the query to a set of records (i.e filter, sort, distinct, etc.)
- [Ash.CustomExpression] Use `Ash.CustomExpression` to extend Ash's expression syntax in a data-layer agnostic way
- [code interface] Code interface functions now support bulk actions, in a "do what I mean" way. For example: `Domain.deactive(post)` can also be `Post |> Ash.Query.filter(active == true) |> Domain.deactive()`

### Improvements:

- [Ash.Actions.Sort] allow providing a stream of records to sort, and performance improvements
- [bulk actions] add `read_action` option to bulk actions (#1088)
- [Ash.stream] support streaming with offset, or even _no_ pagination
- [Ash.DataLayer.Ets] add debug logging, similar to ecto query debug logging
- [Ash.DataLayer.Ets] support update_query, destroy_query and `Ash.Changeset.filter/2`
- [Embedded resources] don't add `autogenerated_id` to embeds if they don't have a primary key
- [Ash.Resource] you can now omit the return type of generic actions, indicating it either succeeds or fails, returning `:ok` or `{:error, error}`
- [Ash.Resource] Generic actions can now accept a Reactor module, running it directly. (#993)
- [Ash.Resource] support `allow_nil_input` dsl option in update/destroy actions (#964)
- [Ash.Resource] The `filter` option can now be supplied multiple times in read actions and in relationships. They will be combined with `and`
- [Ash.Resource] private attributes can now be accepted as action inputs
- [Ash.Expr] is now imported automatically into places you will likely use it, like changes, validations, checks and calculations.
- [Ash.Query] is now required automatically in places you will likely use it, as above
- [sortable?] fields may mark themselves as unusable in sorts by using `sortable? false`
- [sensitive?] calculations and aggregates may now also be marked as `sensitive?`

### Bug Fixes:

- [Ash.Type] apply array type `nil_items?` constraint after item constraints are applied (#1115)
- [Ash.DataLayer.Ets] fix ETS data layer's support for lateral joining
- [bulk actions] ensure transaction is rolled back on data layer errors during streaming
- [bulk actions] set `notify?: true` when `return_notifications?: true` is set
- [Ash.Changeset] `attributes_present?/2` -> `attribute_present?/2`
- [Ash.Filter] don't eager evaluate `type/3` because data layers require type information
- [Ash.Changeset] when comparing identities for `manage_relationship`, we now properly cast the values. Before, `"1"` and `1` were not considered equal for integer primary keys/identity fields
- Many more bug fixes were added, but few are relevant enough to list here
