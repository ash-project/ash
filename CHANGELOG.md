# Changelog

<!-- changelog -->

## [v3.1.3](https://github.com/ash-project/ash/compare/v3.1.2...v3.1.3) (2024-07-11)

### Bug Fixes:

- [bulk actions] ensure that errors in queries do not raise in atomic upgrades/single atomics

- [Ash.Type.Integer] use correct contraint when validating min int (#1298)

- [Ash.Filter] don't refer to private attributes when parsing filter inputs that refer to relationships (#1280)

### Improvements:

- [Ash.Query] add `strict?` option to `Ash.Query.load` (#1302)

## [v3.1.2](https://github.com/ash-project/ash/compare/v3.1.1...v3.1.2) (2024-07-10)

### Bug Fixes:

- [bulk actions] ensure that manual action configurations are honored for bulk actions

## [v3.1.1](https://github.com/ash-project/ash/compare/v3.1.0...v3.1.1) (2024-07-10)

### Bug Fixes:

- [ash.install] installer doesn't need to add spark as a dependency, just run its installer

## [v3.1.0](https://github.com/ash-project/ash/compare/v3.0.16...v3.1.0) (2024-07-09)

### Features:

- [Generators] add `mix ash.install` (call with `mix igniter.install ash`)

- [Generators] add `mix ash.gen.resource`

- [Generators] add `mix ash.gen.base_resource`

- [Generators] add `mix ash.gen.domain`

- [Generators] add `mix ash.extend`

- [Ash.Type.UUIDv7] Add built in `Ash.Type.UUIDv7` type, and `uuid_v7_primary_key` builder

### Bug Fixes:

- [atomics] sort primary key changes ahead of others in atomic changes

- [Ash.Changeet] fix typespec for Changeset.around_transaction/2 (#1292)

- [multitenancy] ensure tenancy is always enforced on create/update/destroy actions

- [loading relationships] lateral join on `from_many? true` relationships

- [calculations] don't reuse calculations/aggregates if `authorize?` is true

- [aggregates] ensure aggregate context is fully configured in `Ash.aggregate`

- [bulk actions] properly transfer changeset.context on streamed batch changesets

- [bulk actions] ensure notifications are dispatched from bulk actions

- [lazy loading] lazy-loading logic for calculations/aggregates was inversed (#1275)

- [error handling] properly match on async task exceptions

- [policies] ensure `context` is available in policy template expressions

- [policies] ensure forbidden errors behave the same when using implicit bulk version of code interface functions

- [manual relationships] compare keys in manual relationships when using 'complex' types (#1270)

- [Ash.Filter] cover more cases in filter input parsing (#1261)

- [has_one relationships] automatically set `from_many?` if a `has_one` has a `sort` applied

- [Ash.Filter] fix match error when synthesizing joins across data layers

- [Ash.DataLayer.Ets] properly support multitenancy when referencing relationships

- [Ash.Type.Union] initialize & validate each subtype of a union

### Improvements:

- [Ash.Type.Enum] allow overriding `cast_stored/2` and `dump_to_native/2`

- [Ash.DataLayer.Simple] support offset in the simple data layer

- [Ash.Changeset] allow after_action hooks in fully atomic changesets

## [v3.0.16](https://github.com/ash-project/ash/compare/v3.0.15...v3.0.16) (2024-06-21)

### Bug Fixes:

- [bulk updates] use the proper opts when calling manual updates in bulk updates

- [pagination] apply pagination at runtime for non lateral join queries

- [multitenancy] consider multitenancy when checking if through-join is unique

- [Ash.Changeset] don't run any `before_action` hooks if changeset is invalidated in prior hook

- [atomic upgrade] only prevent atomic upgrade when hooks were explicitly added

### Improvements:

- [Ash.Error] retain error context on overridden messages

## [v3.0.15](https://github.com/ash-project/ash/compare/v3.0.14...v3.0.15) (2024-06-18)

### Improvements:

- [Ash.Type] add optional `matches_type?/2` callback to `Ash.Type`

- [Ash.Domain] add `backwards_compatible_interface?` option to `use Ash.Domain`

## [v3.0.14](https://github.com/ash-project/ash/compare/v3.0.13...v3.0.14) (2024-06-18)

### Bug Fixes:

- [many-to-many relationships] apply join relationship filter when loading many_to_many relationships

- [Ash.Query] ensure we honor any computed select changes when loading through attributes

### Improvements:

- [Ash.Policy.Authorier] add `subject` and `context` keys to policy context

## [v3.0.13](https://github.com/ash-project/ash/compare/v3.0.12...v3.0.13) (2024-06-17)

### Bug Fixes:

- [parallelism] don't start async limiter tasks if async is disabled

- [Ash.Domain] properly set default timeout to `:infinity`

- [upserts] pass down `identity` when doing upserts, for new feature support

- [Ash.Changeset] ensure that `before_transaction` hook errors fail the operation

- [Ash.Changeset] ensure that `before_transaction` hook errors still trigger `after_transaction` hooks

- [bulk updates] abort bulk updates on before transaction hook errors

## [v3.0.12](https://github.com/ash-project/ash/compare/v3.0.11...v3.0.12) (2024-06-14)

### Bug Fixes:

- [atomic updates] fix expression interpolation for cast_atomic for integer, decimal, float

- [generic actions] set default argument values on generic actions

- [generic actions] support `^arg/1` and similar constructions in filter policies on generic actions

### Improvements:

- [Ash.Resource] set a `module` when validating accepts

## [v3.0.11](https://github.com/ash-project/ash/compare/v3.0.10...v3.0.11) (2024-06-11)

### Bug Fixes:

- [loading through attributes] only apply load through for attributes that are being selected directly

- [relationship loading] ensure we lateral join with `from_many?: true` or any `:many` cardinality relationships

- [create/update/destroy actions] correctly load paginated relationships after create, update, delete (#1229)

- [bulk create/update/destroy] load relationships on bulk operations (#1234)

- [Ash.Type.Atom] return proper `{:ok, value}` from `Ash.Type.Atom.apply_constraints/2`

- [Ash.Filter] fix the compare/2 implementations (#1232)

- [Ash.Filter] return proper value from short-circuit filter hydration

- [Ash.Seed] fix seed not working when :**keep_nil** is generated using seed_input (#1228)

- [Ash.Generator] pass resource to Ash.Seed.seed! in Ash.Generator.seed! (#1227)

### Improvements:

- [Ash.Resource] validate `require_attributes` (error) and `allow_nil_input` (warning) at compile time

- [Ash.Seed] add tenant option to Ash.Seed.seed! (#1230)

## [v3.0.10](https://github.com/ash-project/ash/compare/v3.0.9...v3.0.10) (2024-06-06)

### Bug Fixes:

- [Ash.Union] ensure that union types w/ explicit tags have constraints applied

- [multitenancy] don't update tenant on update, instead enforce it

- [compare/2 validation] Do not compare nil values in `compare` validation (#1223)

- [bulk actions] ensure context is properly set on bulk manual action invocations

### Improvements:

- [Ash.Resource] detect invalid resources placed in relationships on domains verifier

- [Ash.Resource] warn at compile time on types that don't define `atomic_update/2`

## [v3.0.9](https://github.com/ash-project/ash/compare/v3.0.8...v3.0.9) (2024-05-31)

### Bug Fixes:

- [Ash.Filter] use correct boolean operation names in Filter.find/4 (#1214)

- [aggregates] when hydrating nested aggregates, use correct related resource/path pair

- [aggregates] retain `ref_path` when authorizing aggregates

- [relationship loading] ensure that belongs_to relationships are properly not reloaded with `lazy?: true`

- [bulk actions] implement rollback on after hooks for bulk actions

- [bulk actions] check if in transaction before trying to roll it back

### Improvements:

- compatibility with elixir 1.17

## [v3.0.8](https://github.com/ash-project/ash/compare/v3.0.7...v3.0.8) (2024-05-28)

### Bug Fixes:

- [bulk updates] missing `else` caused manual updates not to work with `bulk_update`

- [Ash.Policy.Authorizer] properly compose multiple filter-checks in policy conditions

- [Ash.Policy.Authorizer] properly honor trailing policies that are constantly false

### Improvements:

- [Ash.Notifier] verify notifiers all use the `Ash.Notifier` behaviour

## [v3.0.7](https://github.com/ash-project/ash/compare/v3.0.6...v3.0.7) (2024-05-24)

### Improvements:

- [identities] support `nils_distinct?` on identities

- [identties] support `where` option on `identities`

- [identities] allow calculations in identity keys

## [v3.0.6](https://github.com/ash-project/ash/compare/v3.0.5...v3.0.6) (2024-05-23)

### Bug Fixes:

- [policies] don't raise an error when authorizing against previous values without atomic upgrades

- [calculations] handle subquery-requiring calculations in `calculate/2`, ensuring we have a primary key

- [Ash.DataLayer] resolve mixup between atomic upsert vs update capability of datalayers (#1198)

## [v3.0.5](https://github.com/ash-project/ash/compare/v3.0.4...v3.0.5) (2024-05-23)

### Bug Fixes:

- [mix ash.*] only use `Mix.deps_tree` if defined (its only defined for elixir 1.15+)

### Improvements:

- [atomic upgrade] add `atomic_upgrade?` flag to update/destroy actions

- [atomic upgrade] do not do atomic upgrade by default unless `require_atomic?` is `true`

- [atomic upgrade] allow configuring the read action used by atomic upgrades

## [v3.0.4](https://github.com/ash-project/ash/compare/v3.0.3...v3.0.4) (2024-05-22)

### Bug Fixes:

- [bulk update/destroy] ensure that all notifications are sent

## [3.0.3](https://github.com/ash-project/ash/compare/v3.0.2...3.0.3) (2024-05-22)

### Features:

- [relationship pagination] allow retrieving the count of paginated relationships (#1183)

- [Ash.Reactor] Add `bulk_create` step type.
- [Ash.Reactor] Add `bulk_update` step type. (#1185)

### Bug Fixes:

- [Ash.Actions.Read] properly hydrate and scope sorts with query context

- [Ash.Changeset] handle list of atomic conditions coming from atomic validation implementation (#1194) (#1195)

- [embedded resources] handle nil value for old_values when casting arrays (#1191)

- [Ash.Query] use `Ash.Sort.parse_input/3` in `Ash.Query.sort_input/2`

- [Ash.Resource.Validation.Changing] works correctly in atomics, and can eagerly detect changing (#1178)

- [atomic updates] check the where condition before checking validation atomicity (#1177)

- [bulk actions] don't emit after batch notifications if `notify?: false`

- [Ash.Resource] prefer resource domain over option domain (#1176)

- [bulk update/destroy] don't require domain for empty stream bulk update and destroy (#1175)

- [Ash.Generator] only return valid non nil values items from generator (#1121)

- [bulk destroy] properly validate action when calling bulk destroy

- [code interface] allow all strategies for bulk actions in code interfaces by default
- [code interfaces] honor `get?` for bulk update/bulk destroy

### Improvements:

- [Ash.Query] support anonymous aggregates and calculations in sorts

- [sensitive fields] Implement `show_sensitive?` config (#1180)

- [Ash.Query] support `filter_input` and `sort_input` in `Ash.Query.build/2`

- [Ash.Changeset] add `template_requires_actor` check for changesets

- [bulk update/destroy] don't use queries for streaming if they have hooks

- [Ash.Policy.Check.ChangingAttributes] consider `from: nil` in `changing_attributes/1` check

## [v3.0.2](https://github.com/ash-project/ash/compare/v3.0.1...v3.0.2) (2024-05-15)

### Improvements:

- [Ash.Expr] add pattern matching for clarity on values accepted by `ref/1` and `ref/2`

- [Ash.Expr] add `can_return_nil?/1` callback to Ash expressions, allowing for various optimizations

- [Ash.Type.NewType] raise argument error on unknown options in `Ash.Type.NewType`. Helps with typos & misunderstandings

- [embedded resources] use the `source` configuration for attributes in embedded resources (it was previously just ignored)

- [Ash.Policy.Authorizer] better type specification for checks, to get better autocomplete and compile time validation

- [Ash.Error.Invalid.NoSuchInput] added a `did_you_mean` field and used it in the error message

### Bug Fixes:

- [Ash.Resource] properly persist simple_notifiers (they were being ignored before)

- [code interface] accept single ids in code interface as subject for destroy/update

- [bulk update] ensure that the `changed?` context is set in after action hooks on batches

- [relationships] allow for inferred domains when authorizing join queries

- [Ash.Expr] don't treat `nil` as not a valid value when type casting lists

- [atomic upgrade] keep data's metadata in atomic upgraded update (#1165)

## [v3.0.1](https://github.com/ash-project/ash/compare/v3.0.0...v3.0.1) (2024-05-14)

### Features:

- [Ash.Resource.Change.Builtins] Add `cascade_destroy` to builtin changes.

### Bug Fixes:

- [calculations] calculation eager evaluation bug caused `exists` to eager evaluate when we didn't actually have the related data

- [field policies] fix field policy rewrite errors on non-success cases (#1163)

- [embedded resources] fix embedded resource authorization (#1159) (#1160)

- infinite recursion if query is empty (#1158)

- [Ash.DataLayer.Ets] ensure that changeset filters are honored in ETS destroy/update_query

- [update/destroy actions] don't rollback transactions on stale records, ignore stale records in bulk actions

- [bulk creates] don't check required belongs to fields until after setting them in bulk creation

- [code interface] check require_reference? when generating update code interface (#1152)

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
