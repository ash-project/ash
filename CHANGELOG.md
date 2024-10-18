# Changelog

<!-- changelog -->

## [v3.4.33](https://github.com/ash-project/ash/compare/v3.4.32...v3.4.33) (2024-10-18)




### Bug Fixes:

* apply attribute multitenancy on bulk update queries

* use Decimal.eq? in Ash.Type.Decimal (#1532)

* Reactor: Don't validate `inputs` keys when being transformed. (#1527)

* set argument defaults in fully atomic changesets

* ensure that default values & attribute changes are included in attribute changes

* properly unrelate belongs_to relationships

* ensure unrelated records are removed from the current records list

### Improvements:

* update docs for spark changes

* Conditionally enable transactions on default actions. (#1525)

* ash seed upsert! function (#1522)

* Add compile-time checks for `code_interface` arguments in Resource and Domain (#1523)

## [v3.4.32](https://github.com/ash-project/ash/compare/v3.4.31...v3.4.32) (2024-10-14)




### Improvements:

* use new `:csv` option type from igniter

## [v3.4.31](https://github.com/ash-project/ash/compare/v3.4.30...v3.4.31) (2024-10-14)




### Bug Fixes:

* allow strings in generic action skip_unknown_inputs

## [v3.4.30](https://github.com/ash-project/ash/compare/v3.4.29...v3.4.30) (2024-10-14)




### Bug Fixes:

* add `tracer` option to generic action opts

## [v3.4.29](https://github.com/ash-project/ash/compare/v3.4.28...v3.4.29) (2024-10-13)




### Bug Fixes:

* clear change from atomics as well

* update spark for spark.formatter fixes

* properly invoke notify callback in read actions

### Improvements:

* better ergonomics for atomic updates

* add `private_arguments` option

* validate `Ash.ActionInput.for_action` opts

* allow additional callbacks in `Ash.Type.NewType`

## [v3.4.28](https://github.com/ash-project/ash/compare/v3.4.27...v3.4.28) (2024-10-10)

### Improvements:

- [upserts] support lazy evaluation of skipped upsert records

## [v3.4.27](https://github.com/ash-project/ash/compare/v3.4.26...v3.4.27) (2024-10-10)

### Improvements:

- [upserts] emit StaleRecordError on skipped upsert

## [v3.4.26](https://github.com/ash-project/ash/compare/v3.4.25...v3.4.26) (2024-10-08)

### Bug Fixes:

- [query building] properly hydrate sort with parent context

- [query building] handle nested parent references in runtime expression logic

- [query building] set parent stack when hydrating references in related queries

## [v3.4.25](https://github.com/ash-project/ash/compare/v3.4.24...v3.4.25) (2024-10-07)

### Bug Fixes:

- [igniter] honor --yes or -y option when adding a satsolver

- [query building] don't error on type casting against expressions

## [v3.4.24](https://github.com/ash-project/ash/compare/v3.4.23...v3.4.24) (2024-10-07)

### Bug Fixes:

- [policies] short-circuit policy condition evaluation when checking all conditions

- [query building] properly hydrate aggregate calculations & fields

### Improvements:

- [policies] re-introduce removed behavior to short circuit policy conditions

## [v3.4.23](https://github.com/ash-project/ash/compare/v3.4.22...v3.4.23) (2024-10-03)

### Bug Fixes:

- [loading data] don't rewrite calculation dependencies through not loaded/forbidden fields

- [loading data] honor `reuse_values?` when lazy loading relationships

## [v03.4.22](https://github.com/ash-project/ash/compare/v3.4.21...v03.4.22) (2024-10-01)

### Features:

- [`Ash.Reactor`] Add the ability to specify action context in steps. (#1477)

### Bug Fixes:

- [`Ash.Query`] properly merge query calculations when one side is empty

- [`Ash.Query`] remove expensive calculation reification step that is no longer necessary

- [`Ash.Sort`] handle expression calculations that reference fields in input sorting

- [`Ash.Sort`] properly apply field policies to all filter expressions

- [`Ash.Type.Struct`] better error message on missing `instance_of` constraint on load-through

- [authorization] select minimal data in authorization queries

- [`Ash.Query`] calling `for_read/2..4` should raise an `ArgumentError` when the specified action doesn't exist. (#1479)

- [`Ash.Changeset`] detect non-changing but setting attributes to honor `require_attributes` on update

- [`Ash.Changeset`] set right defaults for `action_select` (#1476)

- [`Ash.Expr`] don't resolve references when falling back to elixir handling for expressions

### Improvements:

- [`Ash.Policy.Authorizer`] disallow 2-tuple expression checks, to resolve ambiguity

- [`Ash.Policy.Authorizer`] rewrite and drastically simplify policy -> solver expression logic

- [`Ash.Changeset`] properly handle bypasses of atomic constraint casting

- [`Ash.Changeset`] ensure that action_select sets attributes to `%Ash.NotLoaded{}`

- [`Ash.Query.Calculation`] add `Ash.Query.Calculation.from_resource_calculation`

- [`Ash.Query.Calculation`] fallback to runtime calculations when expressions aren't supported

- [`Ash.Type.DateTime`] handle iso8601 dates in datetime cast

- [`Ash.DataLayer`] add data layer capability for action select

- [`Ash.Query.Calculation`] inspect calculations in queries more fluidly

- [`Ash.Resource.Igniter`] add_identity for `Ash.Resource.Igniter`

## [v3.4.21](https://github.com/ash-project/ash/compare/v3.4.20...v3.4.21) (2024-09-24)

### Bug Fixes:

- [`Ash`] handle `nil` result in `Ash.first`

- [bulk actions] add checks for `around_transaction` and `around_action` in bulk (#1474)

- [`Ash.Query.Aggregate`] include distinct from queries in aggregate query

- [read actions] reselect required attributes unless `reuse_values?` is `true`

- [`Ash.Changeset`] properly return `{:not_atomic` while applying atomic changes

### Improvements:

- [`Ash.Query.Aggregate`] proper error on unsupported aggregates

## [v3.4.20](https://github.com/ash-project/ash/compare/v3.4.19...v3.4.20) (2024-09-23)

### Bug Fixes:

- [read actions] don't double-load data on bulk update reads

### Improvements:

- [`Ash`] support more formats in `Ash.can`

- [`Ash`] add `validate?` option to `Ash.can`/`Ash.can?`

## [v3.4.19](https://github.com/ash-project/ash/compare/v3.4.18...v3.4.19) (2024-09-21)

### Bug Fixes:

- [`Ash.Resource`] properly generate bypasses with `Ash.Resource.Igniter.add_bypass/2`

### Improvements:

- [`Ash.Sort`] support nested fields in input sorts

- [optimization] optimize the reselection of necessary attributes on lazy loading

- [`Ash.Resource`, optimization] optimize `Ash.Resource.selected?/2` in light of 3.0 changes

## [v3.4.18](https://github.com/ash-project/ash/compare/v3.4.17...v3.4.18) (2024-09-20)

### Bug Fixes:

- [`Ash.Resource.Change.OptimisticLock`] properly increment version in optimist lock's non-atomic branch

- [`Ash.Policy.Authorizer`] ensure that policy group compile time validations are enforced

- [bulk updates] ensure that around_transaction and around_action hooks incur simple updates

## [v3.4.17](https://github.com/ash-project/ash/compare/v3.4.16...v3.4.17) (2024-09-19)

### Bug Fixes:

- [`Ash.Query`] handle more types in `Ash.Query.unload`

- [`Ash.Changeset`] properly escape changeset.select in `Ash.Changeset.ensure_selected` (#1466)

### Improvements:

- [`Ash.Tracer`] add span & telemetry events for running calculations

- [`Ash.Policy.Check.Builtins`] validate action types in `action_type` check

## [v3.4.16](https://github.com/ash-project/ash/compare/v3.4.15...v3.4.16) (2024-09-18)

### Bug Fixes:

- [`Ash.Seed`] ensure Ash.Seed always sets action_select

### Improvements:

- [`Ash.Policy.Authorizer`] properly log successful policy breakdowns with extra info

- [`Ash.Filter`] add `Ash.Filter.fetch_simple_equality_predicate`

## [v3.4.15](https://github.com/ash-project/ash/compare/v3.4.14...v3.4.15) (2024-09-17)

### Improvements:

- [`Ash.Query`] add `load` option to `Ash.Query.for_read`

## [v3.4.14](https://github.com/ash-project/ash/compare/v3.4.13...v3.4.14) (2024-09-17)

### Bug Fixes:

- [`mix ash.gen.domain`] properly detect domains that don't exist yet in `ash.gen.domain`

## [v3.4.13](https://github.com/ash-project/ash/compare/v3.4.12...v3.4.13) (2024-09-17)

### Bug Fixes:

- [`Ash.Changeset`] honor `skip_global_validations?` on fully atomic changesets

- [`Ash.Sort`] ensure calculation context is fully propagated to sort statements

- [`Ash.Policy.Authorizer`] ensure that `resource` context is set for expanding filter descriptions

## [v3.4.12](https://github.com/ash-project/ash/compare/v3.4.11...v3.4.12) (2024-09-16)

### Bug Fixes:

- [`Ash.Seed`] ensure that action_select is set on seeding data

### Improvements:

- [`Ash.Resource.Igniter`] add more resource updating logic

- [`Ash.Resource.Igniter`] add `_new` options for Ash.Resource.Igniter

## [v3.4.11](https://github.com/ash-project/ash/compare/v3.4.10...v3.4.11) (2024-09-13)

### Improvements:

- [igniter] update igniter and fix deprecation warnings

## [v3.4.10](https://github.com/ash-project/ash/compare/v3.4.9...v3.4.10) (2024-09-13)

### Bug Fixes:

- [`mix ash.patch.extend`] properly add all types of extensions in `mix ash.patch.extend`

## [v3.4.9](https://github.com/ash-project/ash/compare/v3.4.8...v3.4.9) (2024-09-13)

### Bug Fixes:

- [field policies] ensure that field policies don't interfere with relationship loading

- [bulk actions] properly merge provided context in atomic bulk actions

- [managed relationships] properly handle rollbacks from `DBConnection` failures for belongs to relationships

- [`Ash.Resource.Igniter`] don't generate doubly nested policies when adding policies in igniter

- [`Ash.Changeset`] fix Ash.Changeset.manage_relationships/4 for list primary keys (#1455)

- [`Ash.Filter`] Handle Ash.Query.filter for array values (#1452)

- [`Ash.Type.Time`] cast embedded time properly (#1451)

- [create actions] require private/non-accepted attributes _after_ before action hooks instead of _before_

- [built in after_action change] we cannot assume that `after_action/1` can be done atomically

  Previously, when you did `change after_action/3` in a resource, we would assume it was safe to be done atomically.
  But because we cannot guarantee that your hook does not access `changeset.data`, it is not safe to make that assumption.

  Instead, you must define a module change, and explicitly define `atomic/3`.

### Improvements:

- [`Ash.Error.Forbidden.Policy`] small improvements for policy breakdown formatting

- [`Ash.Type.Union`] honor a `_union_type` type param when casting unions

- [create/update/destroy actions] add system for `action_select`, which can limit selects from mutations

  Callers can `select` when calling create/update/destroy actions, but those selects were not previously honored
  by data layers. The reason for this is that often actions will require more fields than the fields that the caller
  requests. Now, you can specify `action_select` in the action, and the data layer will honor that.

  Additionally, the new `select_by_default?` flag on attributes causes the attribute to automatically not be selected
  for update actions.

- [attributes] support `select_by_default?` flag on attributes. This defaults to `true`.

## [v3.4.8](https://github.com/ash-project/ash/compare/v3.4.7...v3.4.8) (2024-09-09)

### Bug Fixes:

- [`Ash.Policy.Authorizer`] support passing a forbidden error for policy breakdowns per the docs

- [`Ash.Policy.Authorizer`] don't report the action as the actor for policy breakdowns

- [`Ash.Changeset`] check `changeset.action` before raising a required primary action error

- [bulk actions] ensure proper return types for `:stream` strategy bulk update/destroys

## [v3.4.7](https://github.com/ash-project/ash/compare/v3.4.6...v3.4.7) (2024-09-06)

### Improvements:

- [`Ash.Resource.Igniter`] add `add_bypass` and `add_policy` igniter utilities

## [v3.4.6](https://github.com/ash-project/ash/compare/v3.4.5...v3.4.6) (2024-09-06)

### Bug Fixes:

- [loading relationships] don't select destination attributes that don't exist

- [`Ash.Filter.Runtime`] properly pass actor when running filters at runtime

- [`Ash.Type.Struct`] misplaced curly bracket when handling struct type casting

- [bulk/atomic updates] properly leverage atomic upgrade read action for an update action

### Improvements:

- [`Ash.Policy.Authorizer`] show informative error explaining the use of filter checks with create actions

- [`Ash.Policy.Authorizer`] show the actor's primary key in policy breakdowns

- [`Ash.Policy.Authorizer`] add an expanded description option to checks

- [`Ash.Policy.Authorizer`] use expanded description to display filled in filter templates in policy breakdowns

- [`Ash.Changeset` ] Add `Ash.Changeset.is_valid/1` guard. (#1437)

## [v3.4.5](https://github.com/ash-project/ash/compare/v3.4.4...v3.4.5) (2024-09-05)

### Bug Fixes:

- [update actions] fix type definition for `atomic_upgrade_with`

## [v3.4.4](https://github.com/ash-project/ash/compare/v3.4.3...v3.4.4) (2024-09-05)

## [v3.4.3](https://github.com/ash-project/ash/compare/v3.4.2...v3.4.3) (2024-09-04)

### Bug Fixes:

- [loading relationships] properly await tasks from lazy loading multiple relationships

## [v3.4.2](https://github.com/ash-project/ash/compare/v3.4.1...v3.4.2) (2024-09-04)

### Bug Fixes:

- [soft destroys] honor `return_destroyed?` in soft destroy actions

- [`Ash.Resource.Change`] correctly handle return values of batch callbacks (#1424)

- [read actions] ensure that async limiter is cleared up front

- [bulk creates] honor bulk upsert condition (#1432)

- [bulk updates] ensure that update_defaults are set on streaming updates

- [bulk actions] honor `skip_global_validations?` in bulk actions

- [pagination] honor the `countable` option in pagination

- [read actions] return proper data shape when doing a read in a transaction

- [notifications] ensure that `from` is properly set on all notifications

- [notifications] fix typo in bulk destroy not clearing ash_started_transaction state

- [calculations] traverse calculated relationships when rewriting transient calculation values

- [calculations] don't unload calculation dependencies when `lazy?` is set

- [`Ash.DataLayer.Ets`] handle no_attributes when joining lateral join relationship data

- [`Ash.DataLayer.Ets`] fix ets lateral join source field usage

- [`Ash.DataLayer.Ets`] properly apply distinct in ets

### Improvements with backwards compatibility configurations

These configurations default to the current behavior, but in 4.0 (someday) will
be removed, and the new option will be the only option.

- [pagination] make default page type configurable, defaulting to `:offset`.

```elixir
# set this configuration to adopt the new preferred behavior
config :ash,
  default_page_type: :keyset
```

- [`Ash.Policy.Authorizer`] make read policies more consistent, always preferring to filter over raise

Currently, some read actions can still return a `Forbidden` error, even
though policies are meant to filter out records by default. Now, it will always
filter, unless you set `access_type :strict` in the policy.

```elixir
# set this configuration to adopt the new preferred behavior
config :ash, :policies,
  no_filter_static_forbidden_reads?: false
```

### Improvements:

- [`Ash.Policy.Authorizer`] show an explanation when a forbidden is because no policies applied

- [`Ash.Policy.Authorizer`] error at compile for bypasses that will have no effect

- [`Ash.Resource.ManualRead`] add `load_relationships/5` callback to manual reads

- [`mix ash.gen.resource`] add `uuid-v7-primary-key` option to `mix ash.gen.resource`

- [`Ash.Resource.Change.CascadeUpdate`] add cascade update built in change (#1398)

- [`Ash.Resource.Change.CascadeDestroy`] add `read_action` option to `cascade_destroy`

- [inline aggregates] support `expr` and `expr_type` options when building aggregates

- [create actions] Implement condition for upsert (#1386)

- [optimization] do not add relationship filter when building relationship authorization

- [optimization] don't list telemetry handlers if app is compiling

- [optimization] do not call tracer `set_metadata` with span type that it ignores (#1400)

- [optimization] optimize filter expr traversal

- [optimization] Add a case for handling mapsets in Filter.map (#1427)

- [optimization] Cache always selected fields and use mapsets for building select list (#1428)

- [optimization] add pattern for Ash.Query.Call in Filter.map (#1416)

- [optimization] prevent unnecessary calls to `Ash.load`

- [optimization] cache action known inputs individually

- [optimization] cache action required inputs all together

- [optimization] optimize to avoid inspects in changesets

- [optimization] optimize to avoid expensive `String.valid?` check in uuid type

- [optimization] add `async?` option to calculations, default to false

- [optimization] optimize field checking for loading fields in query

- [optimization] allow functions in tracers for lazy loading metadata

- [optimization] don't start processes for single items in list

- [optimization] Optimize option validation with compile time validators (#1387)

## [v3.4.1](https://github.com/ash-project/ash/compare/v3.4.0...v3.4.1) (2024-08-13)

### Bug Fixes:

- [authorization] properly pass actor, action, tenant etc. to lazy loaded relationships

## [v3.4.0](https://github.com/ash-project/ash/compare/v3.3.3...v3.4.0) (2024-08-12)

### Features:

- [`Ash.Policy.Authorizer`] add policy groups

- [authorization] support `authorize_with` option on `Ash.read`, allowing to error if any forbidden matching data exists

- [`Ash.Resource`] Add `@type t` typespec to resource if it doesn't exist

### Bug Fixes:

- [calculations] remove pattern match error when exceptions come from calculations

- [calculations] resolve nested expression calculation references in runtime filters

- [arrays] Fix error with nil value on structure types (#1380)

- [bulk actions] Pass options without :templated tuple to after_batch (#1376)

- [bulk actions] `after_batch` arguments for `bulk_create` with `return_records?` disabled (#1371)

- [bulk actions] set `upsert?` option when managing relationships in bulk creation

- [`Ash.Resource`] check for `nil` resource_calculation in `Ash.Resource.loaded?/2`

- [`Ash.Expr`] properly consider not-loaded record calculations as `:unknown`

- [atomic updates] handle `nil` in atomic array casting

- [aggregates] respect previously validated-for-action query for aggregates

- [`mix ash.gen.resource`] use `timestamps()` instead of `timestamps`

- [belongs_to relationships] prefer `source_attribute` is required, instead of relationship name

### Improvements:

- [performance] optimizations around allocating strings

- [performance] optimizations around list operations for embedded resources

- [performance] prune calculations made unnecessary by field policies

- [performance] add optimized path for casting embeds when they are simple

- [performance] add `include_embedded_source_by_default?` config to optimize embeds

- [error messages] show proper error message when trying to accept non-writable attributes

- [error messages] add "hints" to `NoSuchInput` errors to make any errors clearer

- [`Ash.Expr`] warn on usage of `== nil`

- [`Ash.Expr`] implement `Comp` for atoms & strings, comparing atoms as strings

- [embedded resources] increase cases where embedded attribute can be updated atomically

- [`Ash.Type.Struct`] support `:fields` constraint on `:struct` type, enabling persistence

- [bulk actions] Warn on bulk action `return_stream?` without any other `return_*?` options enabled. (#1370)

- [calculations] ensure the called calculation function is always in the stacktrace

- [dependencies] remove `:comparable` as a dependency

- [`Ash.Resource`] Add default values to resulting Resource struct (#1364)

## [v3.3.3](https://github.com/ash-project/ash/compare/v3.3.2...v3.3.3) (2024-08-01)

### Bug Fixes:

- delete ash_notifications from pdict after reading

## [v3.3.2](https://github.com/ash-project/ash/compare/v3.3.1...v3.3.2) (2024-08-01)

### Bug Fixes:

- [`Ash.Igniter`] properly parse multiple occurrences of :keep arguments

- [calculations] properly key nested calculations and add additional tests

- [calculations] pass relationship path down when merging query loads

- [`mix ash.codegen`] don't set `--name nil` when calling codegen tasks

- [`Ash.Filter`] fix behavior of synthesized joins across data layers

### Improvements:

- [`mix ash.gen.resource`] add `--timestamps` argument to add timestamps to the resource

## [v3.3.1](https://github.com/ash-project/ash/compare/v3.3.0...v3.3.1) (2024-07-30)

### Bug Fixes:

- [`mix ash.gen.domain`] properly parse domain module in `mix ash.gen.domain`

- [`Ash.Resource.Change`, `Ash.Resource.Validation`] properly handle mixed atomic & non-atomic validations/changes

- [`Ash.Filter`] properly find data layer predicates when name is provided as a string

- [relationships] set `accessing_from` and join read action for many_to_many relationships correctly (#1355)

### Improvements:

- [`Ash.Resource.Change`] implement `change/3` automatically if batch callbacks are defined

## [v3.3.0](https://github.com/ash-project/ash/compare/v3.2.6...v3.3.0) (2024-07-27)

### Features:

- [`Ash.Type.File`] Add Ash.Type.File (#1337)

### Bug Fixes:

- [bulk actions] ensure that statuses are set correctly on bulk actions

- [bulk actions] properly transfer process context(tracers) for bulk actions

- [embedded resources] properly display identity/primary key mismatch error on lists of embeds

- [`Ash.Type.NewType`] apply constraints to NewType even when casting an array (#1341)

- [`Ash.Query`] pass reuse_values? true when loading in Ash.Query.apply_to/2 (#1346)

- [code interfaces] honor `skip_unknown_inputs` in code interfaces

- [notifications] don't gather notifications except for in the top level transaction starter

- [generic actions] support `skip_unknown_inputs` on generic actions

- [atomic updates] ensure `changed?` context is set on atomic changesets (we assume it is `true`, its up to extensions to handle)

- [`Ash.Type.CiString`, `Ash.Type.String`] Update string/ci_string generators to ensure min_length (#1335)

- [`Ash`] handle `{record, action, input}` types in `Ash.can?`

- [bulk actions] only call `batch_change` if it is defined, never `change` in bulk create

### Improvements:

- [`mix ash.gen.resource`] better positional argument handling with igniter

- [`Ash.Expr`] use `:utc_datetime_usec` for `now()` return type

- [`mix ash.install`] don't install sat solver in initial installation

- [`Ash.Policy.Authorizer`] validate that a solver exists at compile time when using policies

- [`Ash.Type.Enum`] Expose type t() on Ash.Type.Enum implementations (#1338)

- [`Ash.Resource`] add :\* as a valid value in `skip_unknown_inputs`

- [`Ash.Resource`] add `skip_unknown_inputs` to individual actions

- [embedded resources] add `skip_unknown_inputs` constraint to embedded resources

- [embedded resources] automatically fall back to a default domain when working with embeds

- [`Ash`] handle 3 tuple in `Ash.can?`

- [`Ash.Error`] add `Ash.Error.error_descriptions`

## [v3.2.6](https://github.com/ash-project/ash/compare/v3.2.5...v3.2.6) (2024-07-22)

### Bug Fixes:

- [bulk actions] fallback to `authorize_with` when authorizing bulk destroy actions

- [bulk actions] don't refer to non-existent `batch_change/4`

### Improvements:

- [bulk actions] Replace incorrect function_exported?-checks in bulk-actions, add has-defs for change modules (#1330)

## [v3.2.5](https://github.com/ash-project/ash/compare/v3.2.4...v3.2.5) (2024-07-22)

### Bug Fixes:

- [destroy actions] apply atomic validations on non-bulk destroy actions

- [`Ash.Policy.Info`] add default to private_fields_policy in Ash.Policy.Info (#1329)

- [relating_to_actor] make `relating_to_actor` built-in-check aware of atomics

- [`Ash.Expr`] remove redundant overload of `Ash.Expr.get_path` (#1328)

- [`Ash.Type.NewType`] cast_input/2 of Ash.Type.NewType returning :ok (#1324)

- [`Ash.Reactor`] warnings emitted by removed reactor behaviour function. (#1325)

### Improvements:

- [bulk actions] add `authorize_with` fallback option to bulk actions

- [`Ash.Policy.Authorizer`] allow policy conditions to be applied inside their block

## [v3.2.4](https://github.com/ash-project/ash/compare/v3.2.3...v3.2.4) (2024-07-18)

### Bug Fixes:

- [transaction hooks] fix warning on transaction hooks violating their semantics

## [v3.2.3](https://github.com/ash-project/ash/compare/v3.2.2...v3.2.3) (2024-07-18)

### Bug Fixes:

- [`mix ash.patch.extend`] properly convert extension string into a module

- [`mix ash.patch.extend`] only display available to extend

- [`mix ash.install`] mix igniter.install ash --example case clause error (#1317)

- [multitenancy] only use attribute for filtering when multitenancy strategy == :attribute

### Improvements:

- [`Ash.Resource.Igniter`] Add `Ash.Resource.Igniter.domain` to get the domain of a resource

## [v3.2.2](https://github.com/ash-project/ash/compare/v3.2.1...v3.2.2) (2024-07-17)

### Features:

- [`Ash.Reactor`] Add ash_step wrapper (#1311)

### Bug Fixes:

- [bulk destroys] honor atomic validations in destroy actions using `filter`

- [Ash.Type.Vector] handle casting nil vectors (#1316)

- [Ash.Type] don't override `nil` handling in `Ash.Type.cast_input/3`

## [v3.2.1](https://github.com/ash-project/ash/compare/v3.2.0...v3.2.1) (2024-07-17)

### Bug Fixes:

- properly honor the `include_nil?` option

- store after_action hooks added outside of changes for atomic upgrade

- don't use `type/3` in string interpolation

- properly pass `include_nil?` from when building query aggregates

### Improvements:

- allow skipping initialization of types in unions

## [v3.2.0](https://github.com/ash-project/ash/compare/v3.1.8...v3.2.0) (2024-07-15)

### Features:

- [field policies] Allow field policies to hide private fields (#1289)

Use the `private_fields :include | :show | :hide` option in the `field_policies` section of your resource to control how private fields are handled by field policies.

For example:

```elixir
# hide all private fields when authorizing
field_policies :hide

# the current behavior. Private fields are ignored by field policies
field_policies :show

# private fields can have field policies like any other field
field_policies :include
```

### Improvements:

- [`Ash.Domain.Igniter`] add `Ash.Domain.Igniter.list_domains/1`

- [`Ash.Resource.Igniter`] add `Ash.Resource.Igniter.list_resources/1`

## [v3.1.8](https://github.com/ash-project/ash/compare/v3.1.7...v3.1.8) (2024-07-14)

### Bug Fixes:

- [bulk actions] use `unpaginated_read` when simulating streaming for low limit queries

## [v3.1.7](https://github.com/ash-project/ash/compare/v3.1.6...v3.1.7) (2024-07-14)

### Bug Fixes:

- [Ash.Query] don't use `:same` return type for most operators

- [Ash.Query] don't use returns as basis type unless explicitly allowed

## [v3.1.6](https://github.com/ash-project/ash/compare/v3.1.5...v3.1.6) (2024-07-14)

### Bug Fixes:

- [Ash.Query] ensure `today` has properly configured returns type

- [Ash.Type] module type apply_constraints for nil fix (#1313)

## [v3.1.5](https://github.com/ash-project/ash/compare/v3.1.4...v3.1.5) (2024-07-14)

### Bug Fixes:

- [Ash.Type] don't specify that `get_path` is a predicate function

### Improvements:

- [Ash.Expr] add `Ash.Expr.determine_type(mod, children)`

- [Ash.Query] add return typing to functions

## [v3.1.4](https://github.com/ash-project/ash/compare/v3.1.3...v3.1.4) (2024-07-13)

### Bug Fixes:

- [code interface] properly omit destroyed result in code interfaces

- [Ash.Type.Integer] properly compare `expr` to `min` with `min` integer constraint

- [Ash.Reactor] Make action ctx-values from reactor-ctx take precedence if set. (#1308)

### Improvements:

- [Ash.Resource.Change] support returning a list of atomics from atomic change callbacks

- [Ash.Type] add `cast_atomic_constraints` callback and use it in core types

- [Ash.Expr] improve type signature for `if/3`

- [Ash.Expr] simpler and/or short circuiting

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
