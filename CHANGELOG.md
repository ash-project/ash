# Change Log

All notable changes to this project will be documented in this file.
See [Conventional Commits](Https://conventionalcommits.org) for commit guidelines.

<!-- changelog -->

## [v2.4.28](https://github.com/ash-project/ash/compare/v2.4.27...v2.4.28) (2022-12-23)




### Bug Fixes:

* import builtins at the action level, not the section level

* fix broken default behavior around managing relationships.

### Improvements:

* fix tests to handle new defaults

* optimize strict checks

## [v2.4.27](https://github.com/ash-project/ash/compare/v2.4.26...v2.4.27) (2022-12-21)




### Bug Fixes:

* properly include module in doc links

* add test for string generator, and fix it

* only allow `nil` in appropriate circumstances in generator

* respect selects in related_query (#464)

* use action name not struct for embedded generators

* use create generators for embedded types

* support dependencies on pruned branch steps

* ensure type compiled for generator

### Improvements:

* fix decimal generator to only compare with floats at generation

* filter min_length strings in generators

## [v2.4.26](https://github.com/ash-project/ash/compare/v2.4.25...v2.4.26) (2022-12-15)




### Improvements:

* update spark

## [v2.4.25](https://github.com/ash-project/ash/compare/v2.4.24...v2.4.25) (2022-12-15)




### Bug Fixes:

* transaction steps use `failure_mode: :stop`

### Improvements:

* better expression detection

## [v2.4.24](https://github.com/ash-project/ash/compare/v2.4.23...v2.4.24) (2022-12-10)




### Improvements:

* more dependable notifications, support custom notifications better

## [v2.4.23](https://github.com/ash-project/ash/compare/v2.4.22...v2.4.23) (2022-12-08)




### Improvements:

* add more transaction types

## [v2.4.22](https://github.com/ash-project/ash/compare/v2.4.21...v2.4.22) (2022-12-08)




### Bug Fixes:

* depend on latest spark

* different formats for extensions

### Improvements:

* add `:struct` (`Ash.Type.struct`) type

## [v2.4.21](https://github.com/ash-project/ash/compare/v2.4.20...v2.4.21) (2022-12-08)




### Bug Fixes:

* don't lock specific nimble options version

### Improvements:

* replace doc links in sidebar also

* better doc replacement behavior

* dep updates & new aggregate types avg/min/max/custom

## [v2.4.20](https://github.com/ash-project/ash/compare/v2.4.19...v2.4.20) (2022-12-07)




### Bug Fixes:

* more matching fixes on manual relationships

* handle manual relationship load results when building query

### Improvements:

* only set `defaults` when attribute is actually set

* Pass context into query and changeset for_... functions. (#460)

## [v2.4.19](https://github.com/ash-project/ash/compare/v2.4.18...v2.4.19) (2022-12-04)




### Improvements:

* set context once, early

## [v2.4.18](https://github.com/ash-project/ash/compare/v2.4.17...v2.4.18) (2022-12-01)




### Bug Fixes:

* set proper types in transaction reasons

## [v2.4.17](https://github.com/ash-project/ash/compare/v2.4.16...v2.4.17) (2022-12-01)




### Improvements:

* support data layers implementing transaction level hooks

## [v2.4.16](https://github.com/ash-project/ash/compare/v2.4.15...v2.4.16) (2022-11-30)




### Bug Fixes:

* properly authorize manage relationship calls

### Improvements:

* don't run calculation queries if no records were returned

* don't run queries with `limit: 0`

* more readable flow charts

* `not_found_error?` option on `get?: true` `read` flow steps

## [v2.4.15](https://github.com/ash-project/ash/compare/v2.4.14...v2.4.15) (2022-11-29)




### Improvements:

* add `not_found_error?` option to interface builder and when calling

## [v2.4.14](https://github.com/ash-project/ash/compare/v2.4.13...v2.4.14) (2022-11-29)




### Bug Fixes:

* properly handle arguments with default values in code interface

## [v2.4.13](https://github.com/ash-project/ash/compare/v2.4.12...v2.4.13) (2022-11-29)




### Bug Fixes:

* don't raise a backwards incompatible error message on certian changeset functions

* properly apply managed relationships on manual actions

* properly pass `resource` option in filter policies

## [v2.4.12](https://github.com/ash-project/ash/compare/v2.4.11...v2.4.12) (2022-11-25)




### Bug Fixes:

* don't revisit nodes in the ash engine

* properly map to `:destroy` step names

* handle `Ash.Flow.Step.Destroy` in path matchers

* resolve issue with `authorize_unless` and filter checks

* fix pattern match error in manage relationship reduce

### Improvements:

* optimize various solver boolean optimizations

* more comprehensively remove unnecessary clauses

* prevent changing attributes and arguments after action validation

## [v2.4.11](https://github.com/ash-project/ash/compare/v2.4.10...v2.4.11) (2022-11-22)




### Bug Fixes:

* fix typespec for Api.Info.trace_name/3

### Improvements:

* add error context to error creation / normalisation (#440)

* update hexdocs processor to link guides on hexdocs

## [v2.4.10](https://github.com/ash-project/ash/compare/v2.4.9...v2.4.10) (2022-11-21)




### Improvements:

* return invalid primary key errors for `Api.get` when the input can't be cast

* much more readable errors when building loads

* better check module validation

## [v2.4.9](https://github.com/ash-project/ash/compare/v2.4.8...v2.4.9) (2022-11-21)




### Bug Fixes:

* reselect relationship source fields when loading relationships

* make plug an optional dependency of Ash

## [v2.4.8](https://github.com/ash-project/ash/compare/v2.4.7...v2.4.8) (2022-11-19)




### Bug Fixes:

* fix `where` and `or_where` implementation

## [v2.4.7](https://github.com/ash-project/ash/compare/v2.4.6...v2.4.7) (2022-11-19)




### Improvements:

* add `where` and `or_where` to expr

## [v2.4.6](https://github.com/ash-project/ash/compare/v2.4.5...v2.4.6) (2022-11-19)




### Improvements:

* mark manual action modules as modules

## [v2.4.5](https://github.com/ash-project/ash/compare/v2.4.4...v2.4.5) (2022-11-19)




### Bug Fixes:

* properly pass a map to manual action context

* destroy action types default to accepts [] (#453)

## [v2.4.4](https://github.com/ash-project/ash/compare/v2.4.3...v2.4.4) (2022-11-18)




### Bug Fixes:

* various typespec/build fixes

* handle tuples properly in some cases in expression syntax

* Add missing `[:ash, :query]` telemetry (#451)

### Improvements:

* improve runtime expression running

* add default description for filter checks

* validate that modules exist and are documented when referenced

## [v2.4.3](https://github.com/ash-project/ash/compare/v2.4.2...v2.4.3) (2022-11-15)




### Bug Fixes:

* don't incur compile time dependency for resources used as types

* allow for not-yet-compiled resources as Ash types

* properly raise error on invalid type.

### Improvements:

* optimize sat solving

## [v2.4.2](https://github.com/ash-project/ash/compare/v2.4.1...v2.4.2) (2022-11-03)




### Bug Fixes:

* properly set error vars with list constraints

* when creating changesets/queries that already have arguments, revalidate arguments

### Improvements:

* update to latest spark

* support statically configured upsert actions

* add more builders

## [v2.4.1](https://github.com/ash-project/ash/compare/v2.4.0...v2.4.1) (2022-10-31)




### Bug Fixes:

* fix depend on resources to depend on each resource

* allow references on both sides of `in`

* properly upsert all explicitly changed fields

* traverse maps for `template_references_actor?/1`

### Improvements:

* replace templates in change/preparation/validation opts

## [v2.4.0](https://github.com/ash-project/ash/compare/v2.3.0...v2.4.0) (2022-10-31)




### Features:

* support anonymous functions in DSL

These include:
    
    - custom create/read/update/destroy actions
    - changes
    - preparations
    - validations
    - calculations
    - manual relationships
    
    See the respective DSL guides for more.

### Bug Fixes:

* don't add required belongs_to error if changeset is invalid (#437)

* don't lazy load when managing relationships

### Improvements:

* support anonymous functions for various behaviour based options

* add more ergonomic manual action definitions.

* more additions to the resource builder, update spark

## [v2.3.0](https://github.com/ash-project/ash/compare/v2.2.0...v2.3.0) (2022-10-27)




### Features:

* Ash.PlugHelpers: standardise conn interface for actors/tenants. (#432)

* add `Ash.Resource.Builder`, the start of DSL builder utilities of rextension writing

### Bug Fixes:

* DataLayer: incorrect typespec for `run_query/2` callback. (#431)

* in Ash.Seed, don't try to update a non-loaded record

* properly load manual to_one relationships

* properly compare against decimal values

### Improvements:

* pass tenant to calculation query explicitly

* allow using `get_path/2` by name, as well as bracket access

* SVG, PDF, PNG, Markdown and plain mermaid formats (#428)

* optimize nested `exists` filter statements

* support floats & decimals in the `compare` validation

## [v2.2.0](https://github.com/ash-project/ash/compare/v2.1.0...v2.2.0) (2022-10-21)




### Features:

* add `Ash.Api.Info.depend_on_resources/1` to get the list of resources at compile time

### Bug Fixes:

* don't attempt to re-authorize access to already retrieved records in `Api.load/2`

### Improvements:

* when returning a page, choose keyset if `before` or `after` was supplied

* add keysets to records if any action supports keysets

* show conditions in policy breakdowns

## [v2.1.0](https://github.com/ash-project/ash/compare/v2.0.0...v2.1.0) (2022-10-19)




### Features:

* Custom short names for types (#423)

A compile env can be set to allow customizing the available type short names. This supports two things:

1. Adding custom type short names, like `attribute :price, :money` mapping to `MyApp.Type.Money`
2. Overriding the builtin type short names, like `attribute, :price, :string` mapping to a custom string type implementation (there is likely no reason to do this)
Commit with unknown type in: feat: Custom short names for types (#423)

See the docs for `Ash.Type` for more information

* add `now()` to expressions

### Bug Fixes:

* set defaults before running changes

### Improvements:

* sort relationship in order of input when managing it

This helps with things like https://github.com/ash-project/ash_phoenix/issues/57
which involve rendering the relationship value after editing it. Retaining
the order allows direct reuse without any gymnastics


## [v2.0.0](https://github.com/ash-project/ash/compare/v1.53.3...v2.0.0) (2022-10-17)


### Features:

* basic livebook generator and mix task (#420)

* mermaid class diagram mix task (#417)

### Bug Fixes:

* properly lateral join when possible

* use `prepend?: true` when applying relationship sorts

* don't miss dependencies in flow diagrams

* fix deps finding in flow charts & flows

* properly load calcs/aggs on manual relationships

* properly load nested manual relationships

* allow overriding validation message on a list of errors (#412)

* reraise errors on task error

* don't show dependencies for run_flow steps in expanded view(they are duplicates)

### Improvements:

* don't eager evaluate `type/2`

* support depending on requests that will be added

* support dynamic action steps in `Ash.Flow`

* add `prepend?: true` option to sort

* use `simple_equality?/0` to allow for optimized equality checking

* mermaid mix task for ER diagrams (#415)

* try to resolve flaky tests by not using named tables for private ets resources

* better unknown error handling

* allow passing query or changeset in can/can?/4 (#410)

## [v2.0.0-rc.15](https://github.com/ash-project/ash/compare/v2.0.0-rc.14...v2.0.0-rc.15) (2022-10-10)




### Bug Fixes:

* handle upsert_identity better with ets/mnesia

* always set source on attributes

### Improvements:

* Improve error when actions reject and accept keys overlap (#405)

* update to latest spark

## [v2.0.0-rc.14](https://github.com/ash-project/ash/compare/v2.0.0-rc.13...v2.0.0-rc.14) (2022-10-07)




### Features:

* list arguments for resource actions in class diagrams (#399)

### Bug Fixes:

* fix chart links rendering

* make `loading?/2` know about calcs and aggs

* properly set source on attributes

* policy fixes from pair session (#403)

* don't evaluate expressions incorrectly after casting

### Improvements:

* add `type` function to ash core

* Allow a single `where` condition for validations (#407)

* haltable flows, branch step type

* simplify async task strategy

* clean up new create authorization simplification

* remove the need for `SetTypes`

* add some info to policy errors

* experimental support for calcualtions accepting expression arguments

* various Ash.Flow improvements, including returning the new `Ash.Flow.Result`

## [v2.0.0-rc.13](https://github.com/ash-project/ash/compare/v2.0.0-rc.12...v2.0.0-rc.13) (2022-10-04)




### Features:

* `show_private?` option for diagrams (#396)

* generate mermaid entity relationship diagrams from a given api (#376)

### Bug Fixes:

* add back in `new/2` to Changeset

* properly load nested calcs

* switch from no_depend_modules in most places

* properly display compare/2 error message

* use the short type for aggregate types

* `kind_to_type/2` returns tagged tuple

### Improvements:

* allow select/load callbacks for calcs to return irrelevant keys

* optimize load equality matching by not using `Comp`

* Forbid reserved field names (#388)

* validate accepted and rejected attributes in actions (#395)

* support zero argument functions in `compare/2`

## [v2.0.0-rc.12](https://github.com/ash-project/ash/compare/v2.0.0-rc.11...v2.0.0-rc.12) (2022-09-30)




### Improvements:

* optimize for `relates_to_actor_via`

## [v2.0.0-rc.11](https://github.com/ash-project/ash/compare/v2.0.0-rc.10...v2.0.0-rc.11) (2022-09-29)




### Bug Fixes:

* use `at_path` when parsing `Exists`

* properly require a condition of a following bypasses

* don't transform == nil to is_nil automatically

* pass path down to keyword list errors

### Improvements:

* optimize relates_to_actor_via checks

## [v2.0.0-rc.10](https://github.com/ash-project/ash/compare/v2.0.0-rc.9...v2.0.0-rc.10) (2022-09-28)




### Bug Fixes:

* bad pattern in `filter.ex`, fix dialyzer

* attempt to evaluate filter checks for strict checks

* only return errors when there actually are errors

* return an error if `data_layer_query/2` is given a query with errors

* various fixes with complex policy statements

* ensure fields selected in-line when loading calcs

* handle statically false conditions in filter logic

* cast embedded datetimes properly

* Ash.Calculation: fix return type for `load/3` callback. (#384)

* warn instead of raise on `:replace` usage

* handle var_args expression with literal args

### Improvements:

* catch more cases in preflight authorization checks

* lazily set required loads/selects for calcs/sorts

* reselect any necessary fields when loading calcs

* set context when creating related filters allowing checks like `filtering_on`

* simplify filter statements further

* don't overconstraint filters on related data

* any filter being statically true means `:authorized`

* properly mark conditions w/ access_type

* use `IsNil` instead of `Eq` when either side is `nil`

* handle string dates for embeds

* remove __timestamps__ in favor of simpler macro

## [v2.0.0-rc.9](https://github.com/ash-project/ash/compare/v2.0.0-rc.8...v2.0.0-rc.9) (2022-09-21)




### Bug Fixes:

* fix `replace_relationship` type

## [v2.0.0-rc.8](https://github.com/ash-project/ash/compare/v2.0.0-rc.7...v2.0.0-rc.8) (2022-09-21)




### Bug Fixes:

* properly handle args/nested expression for tuple calcs

* add a case for calculations as tuples in expr filters

* return count, not {:ok, count}

* bad return value when async fetching counts

* remove dbg() call

### Improvements:

* update to latest spark, support dsls in resource info

* deprecate `:replace` in favor of `:append_and_remove`

* add `loading?/1` query helper

* add `loading/1` built in check

## [v2.0.0-rc.7](https://github.com/ash-project/ash/compare/v2.0.0-rc.6...v2.0.0-rc.7) (2022-09-15)




### Bug Fixes:

* `nil` casts as any type (sort of)

* return `nil` on `nil` inputs for length/1

* properly reraise errors raised in tasks

* properly return errors from tasks

* use `Comp.equal?/2` when finding loaded data matches

## [v2.0.0-rc.6](https://github.com/ash-project/ash/compare/v2.0.0-rc.5...v2.0.0-rc.6) (2022-09-15)




### Bug Fixes:

* properly error on types when evaluating expressions at runtime

* properly surface errors all the way from runtime filters

* properly catch errors when running expressions at runtime

### Improvements:

* Implement length function (#379)

## [v2.0.0-rc.5](https://github.com/ash-project/ash/compare/v2.0.0-rc.4...v2.0.0-rc.5) (2022-09-14)




### Bug Fixes:

* inspect the match for default message

### Improvements:

* validate aggregate paths supported

* add `filterable?` option to relationships

* add data layer capability for aggregate relationships & filter relationships

* add guide on manual relationships

## [v2.0.0-rc.4](https://github.com/ash-project/ash/compare/v2.0.0-rc.3...v2.0.0-rc.4) (2022-09-12)




### Bug Fixes:

* fix keyset pagination ordering bug

* short names are snake cased

* properly do pagination

* handle pins in exists

* add better error for `exists/2`

* use root_resource for `related` path in filter

* add `match/3` to upgrading guide

* set root_resource in `exists` parsing

* error fetching relationships in filter expressions

* filter check typespecs

### Improvements:

* add `aggregate_type/2` helper

* make two queries for full keyset pagination support

## [v2.0.0-rc.3](https://github.com/ash-project/ash/compare/v2.0.0-rc.2...v2.0.0-rc.3) (2022-09-06)




### Bug Fixes:

* runtime filter handle new relationship shape

### Improvements:

* add `exists/2` expression

## [v2.0.0-rc.2](https://github.com/ash-project/ash/compare/v2.0.0-rc.1...v2.0.0-rc.2) (2022-09-04)




### Bug Fixes:

* the semantics of `forbid_unless` were not wrong

## [v2.0.0-rc.1](https://github.com/ash-project/ash/compare/v2.0.0-rc.0...v2.0.0-rc.1) (2022-09-04)
### Bug Fixes:

* `forbid_unless` expression compilation

* fix runtime filter join simulation for multiple rows

## [v2.0.0-rc.0](https://github.com/ash-project/ash/compare/v1.53.3...v2.0.0-rc.0) (2022-09-04)

### Bug Fixes:

* Initial Ash 2.0.0-rc.0 release!

## [v1.53.3](https://github.com/ash-project/ash/compare/v1.53.2...v1.53.3) (2022-08-22)




### Bug Fixes:

* False default value for argument is nil in changeset (#364)

* ignore belongs_to in preflight attribute check

* clean up relationship validation logic

* clean up logic around preflight belongs_to validation

### Improvements:

* add `value_is_key` option for managed relationships

* Replace usage of Timex.shift with builtin Calendar functions (#362)

* handle required but not accepted values better

## [v1.53.2](https://github.com/ash-project/ash/compare/v1.53.1...v1.53.2) (2022-08-10)




### Bug Fixes:

* persist a nil actor properly

## [v1.53.1](https://github.com/ash-project/ash/compare/v1.53.0...v1.53.1) (2022-08-10)




### Bug Fixes:

* properly set authorize?: false on runtime filter

* explicitly don't authorize the runtime filter authorization logic

* fix eager function/operator evaluation

* scrub values properly, same as last bug

* map update bug when sanitizing boolean expressions

* fixs runtime filter fallbacks

### Improvements:

* support `authorize?` as a changeset option

* add `actor_present` policy

* add `error?` option to get

* fix various operator evaluators

## [v1.53.0](https://github.com/ash-project/ash/compare/v1.52.0-rc.22...v1.53.0) (2022-08-04)




### Bug Fixes:

* (attempt) to fix calc loading issue

## [v1.52.0-rc.22](https://github.com/ash-project/ash/compare/v1.52.0-rc.21...v1.52.0-rc.22) (2022-08-03)




### Bug Fixes:

* actually use `warn_on_empty?` config

* check for actor on query/changeset for actor context

* pass actor opt down

* don't skip setting tenant when actor is present

* don't use `apply/3` on kernel macros

* small bug in DSL transformer manipulation.

* && and || don't short-circuit to `nil`

* `{:ok, _}` -> `{:known, _}` when evaluating operators

* fix bad evaluation case for operators

* ensure we only take unique related records when lazy loading

### Improvements:

* add warnings to DSL transformer returns

* warn on empty registries

* better sanitization around sensitive attributes in filters

* change `always_authorize?` to `authorize` for multiple options

* add error message for manual action missed

## [v1.52.0-rc.21](https://github.com/ash-project/ash/compare/v1.52.0-rc.20...v1.52.0-rc.21) (2022-07-19)




### Bug Fixes:

* use `Map.get/2` when getting paths if the value is a struct

### Improvements:

* add || and && operators

* sort parsing helpers

* add `Ash.Sort.parse_input!/2`

* add `transfer_context/1` and `get_context_for_transfer/0`

* add process-based actor, tenant and query/changeset context

* add `always_authorize?` and `require_actor?` to api config

* support paths in `actor/1`

## [v1.52.0-rc.20](https://github.com/ash-project/ash/compare/v1.52.0-rc.19...v1.52.0-rc.20) (2022-07-14)




### Features:

* add can?/4 policy utility (#349)

* add can?/4 policy utility

### Improvements:

* add default guide to doc_index

## [v1.52.0-rc.19](https://github.com/ash-project/ash/compare/v1.52.0-rc.18...v1.52.0-rc.19) (2022-07-13)




### Bug Fixes:

* make mnesia and ets work properly when sharing tables

* make updates properly merge with mnesia and ets

* `attribute_writable?` also makes it public

### Improvements:

* code_interface optional arguments

* improve behavior of `lazy?: true` option

## [v1.52.0-rc.18](https://github.com/ash-project/ash/compare/v1.52.0-rc.17...v1.52.0-rc.18) (2022-07-10)




### Bug Fixes:

* fix doc links and include in release

## [v1.52.0-rc.17](https://github.com/ash-project/ash/compare/v1.52.0-rc.16...v1.52.0-rc.17) (2022-07-06)




### Bug Fixes:

* add back in `writable?` option to relationships, and add `attribute_writable?` to `belongs_to`

* don't rescue arbitrary exception/exits

### Improvements:

* add back in DSL docs

* add `match_other_defaults?` to attribute

## [v1.52.0-rc.16](https://github.com/ash-project/ash/compare/v1.52.0-rc.15...v1.52.0-rc.16) (2022-07-05)




### Bug Fixes:

* fix return type for `dump/3` ecto type

* `load/3` returns `{:ok, value} | :error`

### Improvements:

* remove relationship writability, as it all happens through arguments now

* repurpose `writable?` on `belongs_to` to make the attribute writable

## [v1.52.0-rc.15](https://github.com/ash-project/ash/compare/v1.52.0-rc.14...v1.52.0-rc.15) (2022-06-28)




### Bug Fixes:

* ensure type is always set on attributes

## [v1.52.0-rc.14](https://github.com/ash-project/ash/compare/v1.52.0-rc.13...v1.52.0-rc.14) (2022-06-28)




### Bug Fixes:

* don't try to read files that don't exist

### Improvements:

* new timeout error message and test it

## [v1.52.0-rc.13](https://github.com/ash-project/ash/compare/v1.52.0-rc.12...v1.52.0-rc.13) (2022-06-27)




### Bug Fixes:

* bad return value for `destroy!` + `return_notifications?: true`

* use digraph to order transformers

* things breaking due to stricter expectations on type function inputs

* depend on all entries in registry

### Improvements:

* `Ash.Generator`

* add `Ash.Seed` module with seed helpers

* add basic type handling for non embedded resources

* better transformer ordering error

* don't pay massive costs of a function undefined error

* optimize related resource inclusion check

## [v1.52.0-rc.12](https://github.com/ash-project/ash/compare/v1.52.0-rc.11...v1.52.0-rc.12) (2022-06-14)




### Bug Fixes:

* don't disable lexical tracker for extensions

* properly set the `changed?` context

* always return all notifications if `return_notifications?: true`

* read file at compile time for doc index

* when casting atom -> string, stringify it first

### Improvements:

* add resource to notification warning

* add `config :ash, :pub_sub, debug?: true`

* add `from` to notification, and `notification_metadata` to api

## [v1.52.0-rc.11](https://github.com/ash-project/ash/compare/v1.52.0-rc.10...v1.52.0-rc.11) (2022-06-03)




### Bug Fixes:

* move preparation init to runtime

* don't automatically url encode keyset values

* fixed bug where embedded resources would always provide defaults, not allowing you to declare your own (primary?) actions (#339)

* keyset pagination counts all rows

* fetch items closest to cursor by reversing keyset sort

* keyset + before results must be reversed

### Improvements:

* add `identity_priority` and `use_identities` option to manage_relationship

* support limit in simple data layer

* add `key` to `InvalidKeyset` error

## [v1.52.0-rc.10](https://github.com/ash-project/ash/compare/v1.52.0-rc.9...v1.52.0-rc.10) (2022-05-30)




### Improvements:

* better error message on invalid keyset

* added options to the built-in function `relate_actor/1` (#332)

* add `:_pkey` shortcut in pub_sub

* validate `pre_check_with` is set for ets/mnesia identities

* clearer and raised error message on changeset action mismatch

* accept atoms when casting strings

## [v1.52.0-rc.9](https://github.com/ash-project/ash/compare/v1.52.0-rc.8...v1.52.0-rc.9) (2022-05-23)




### Bug Fixes:

* rename `interval` to `duration_name`

* Fix concat (#326)

* Make get and get! consistent with what they raise when no record found (#325)

### Improvements:

* specify that upserts could be related at creation

## [v1.52.0-rc.8](https://github.com/ash-project/ash/compare/v1.52.0-rc.7...v1.52.0-rc.8) (2022-05-18)




### Bug Fixes:

* add resource/action to policy error context

## [v1.52.0-rc.7](https://github.com/ash-project/ash/compare/v1.52.0-rc.6...v1.52.0-rc.7) (2022-05-18)




### Bug Fixes:

* don't ignore lazy load option

## [v1.52.0-rc.6](https://github.com/ash-project/ash/compare/v1.52.0-rc.5...v1.52.0-rc.6) (2022-05-18)




### Bug Fixes:

* return `{:ok, nil}` on nil cast for strings

## [v1.52.0-rc.5](https://github.com/ash-project/ash/compare/v1.52.0-rc.4...v1.52.0-rc.5) (2022-05-17)




### Improvements:

* move ash_policy_authorizer into core as `Ash.Policy.Authorizer`

## [v1.52.0-rc.4](https://github.com/ash-project/ash/compare/v1.52.0-rc.3...v1.52.0-rc.4) (2022-05-17)




### Bug Fixes:

* run after_action in create properly

## [v1.52.0-rc.3](https://github.com/ash-project/ash/compare/v1.52.0-rc.2...v1.52.0-rc.3) (2022-05-17)




### Bug Fixes:

* require calculations specified on resource load

## [v1.52.0-rc.2](https://github.com/ash-project/ash/compare/v1.52.0-rc.1...v1.52.0-rc.2) (2022-05-13)




### Bug Fixes:

* ensure that the default accept is used

* distinct before limit and offset

* add distinct in data_layer_query

* merge calculations when merging loads

* add `no_depend_modules` for changes/validations

* match on `:unknown` not `{:ok, :unknown}`

* run calc in data layer if it returns `:unknown`

* don't ignore lexical tracker on modules in DSL

* don't treat single actions of a type as primary

* render contributor images correctly in hexdocs (#321)

* go back to old method of checking for resource

* properly load from `load` statement in calculations

* send notifications in all cases

* use unpaginated read when loading

* properly handle errors in mnesia transactions

* default custom steps to be async?: false

* get tests/dialyzer passing

### Improvements:

* work on module dependencies

* use new `no_depend_modules` everywhere

* add `no_attributes?` relationships

* add manual read actions

* calculation values from requests

* small optimizations

* more flow features/fixes, debug step

* work on transaction flow steps

## [v1.52.0-rc.1](https://github.com/ash-project/ash/compare/v1.52.0-rc.0...v1.52.0-rc.1) (2022-04-19)




### Bug Fixes:

* Handle date type cast_input with nil value (#311)

* fix expression logic

* don't throw away timeout exit

* timeouts @ the engine, not the parent process

* timeout logic was timing out after the fact

* uniqueify `list_refs` even further

* flaky test issue

* Enforce unique action names (#308)

* pass tenant option to requests properly

* Fix typespecs in Ash.Api (#307)

* fix resource relationship validation

* fix paths for load in flow

* aggregate/calculation filter issues

* show error message in `NoSuchResource`

* import builtin preparations in global preparations block

### Improvements:

* `load` on `cast_stored` in embedded type

* add descriptions to mermaid charts/flow

* tons of engine/timeout improvements

* implement NaiveDateTime type (#312)

* Improve usability of finding by primary key (ID) (#294)

* Add time type matching existing date type (#310)

* flow -> mermaid chart

* flow tenants

* fix nested map statements in flow

* add dynamic allow list

* uniqify list_references

* set default timeout to 30_000

* remove coverage from CI

* fully deprecate the `resource` entity

* add eager validate identities

* percolate `nil` values in operators in ash expression language (like SQL)

* add `return_destroyed?` option

* add `api` option to relationships

* make default actions and primary actions far more explicit

* better error messages on unknown

* better loading behavior for managed relationships

* add lazy? option for loading

* show value in atom error list

* add `modify_query` callback

* add overview

* add build_entity!

* properly parse `{:_ref, path, name}`

* add `deselect` to build

* validates attributes and relationships have unique names (#300)

* validate no embeds in api

## [v1.52.0-rc.0](https://github.com/ash-project/ash/compare/v1.51.2...v1.52.0-rc.0) (2022-03-25)




### Features:

* add `Ash.Flow`

* support recursive DSL entities.

* manual relationships

### Bug Fixes:

* add `load` option convenience for reads/code interface

* handle errors in all action types where `changeset` wasn't resolved

* always sanitize requests before we spawn them

* context name in loading manual relationships

* get aggregate query from proper engine path

* handle error case in create

* don't require attributes if an argument overrides them

* fix hanging issue when adding engine requests

* don't require `writable?: false` attributes

* pull aggregate values properly

* fix nested section configs having wrong path

* don't rescue errors in resource_formatter

* add `input/2` to resource modules

* move back to more efficient formatter

* make the formatter safer, again

* typo in changeset.ex (#291)

### Improvements:

* properly attach authorization_filters to loaded items

* add `ref` template helper

* add transaction steps to flow

* unimport to avoid name collisions in nested DSLs

* disable lexical tracker when expanding aliases

* temporarily move init to runtime for changes

## [v1.51.2](https://github.com/ash-project/ash/compare/v1.51.1...v1.51.2) (2022-02-17)




### Bug Fixes:

* don't blow away sections when formatting

* properly reorder sections in the formatter

## [v1.51.1](https://github.com/ash-project/ash/compare/v1.51.0...v1.51.1) (2022-02-17)




### Bug Fixes:

* solve reorder bugs in formatter

## [v1.51.0](https://github.com/ash-project/ash/compare/v1.50.21...v1.51.0) (2022-02-14)




### Features:

* add `source` option to attributes

## [v1.50.21](https://github.com/ash-project/ash/compare/v1.50.20...v1.50.21) (2022-02-14)




### Improvements:

* add `cast_in_query?/0` to `Ash.Type`

## [v1.50.20](https://github.com/ash-project/ash/compare/v1.50.19...v1.50.20) (2022-02-11)




### Improvements:

* small data layer improvements

## [v1.50.19](https://github.com/ash-project/ash/compare/v1.50.18...v1.50.19) (2022-02-07)




### Bug Fixes:

* include a missing module

* properly set filterability on attributes

## [v1.50.18](https://github.com/ash-project/ash/compare/v1.50.17...v1.50.18) (2022-02-07)




### Bug Fixes:

### Improvements:

* initial implementation of ash resource formatter

* ensure no reserved names can be used as constraints

## [v1.50.17](https://github.com/ash-project/ash/compare/v1.50.16...v1.50.17) (2022-01-31)




### Improvements:

* optimize `if` and `is_nil` functions

## [v1.50.16](https://github.com/ash-project/ash/compare/v1.50.15...v1.50.16) (2022-01-24)




### Bug Fixes:

* use `ash_struct_fields` to accumulate schema struct field defaults

## [v1.50.15](https://github.com/ash-project/ash/compare/v1.50.14...v1.50.15) (2022-01-19)




### Bug Fixes:

* don't call add_aggregates w/ a map

* allow new `filter` pattern in typespec

### Improvements:

* add `where` to `change`

* support data layers bulk adding aggregates

## [v1.50.14](https://github.com/ash-project/ash/compare/v1.50.13...v1.50.14) (2021-12-21)




### Bug Fixes:

* fix recursion in `do_reverse_relationship_path/3`

### Improvements:

* add more authorizer state management

* customizable exception for authorizers

## [v1.50.13](https://github.com/ash-project/ash/compare/v1.50.12...v1.50.13) (2021-12-21)




### Bug Fixes:

* properly construct reverse relationship paths

### Improvements:

* cover more potential cases in filter parsing

## [v1.50.12](https://github.com/ash-project/ash/compare/v1.50.11...v1.50.12) (2021-12-19)




### Bug Fixes:

* support new versions of ecto's struct fields

* fixes for elixir_sense plugin

## [v1.50.11](https://github.com/ash-project/ash/compare/v1.50.10...v1.50.11) (2021-12-13)




### Improvements:

* add elixir_sense extension, to be merged when ready (#275)

## [v1.50.10](https://github.com/ash-project/ash/compare/v1.50.9...v1.50.10) (2021-12-08)




### Improvements:

* add `Ash.DataLayer.Simple.set_data/2`

* complete mutually_exclusive_and_collectively_exhaustive logic

## [v1.50.9](https://github.com/ash-project/ash/compare/v1.50.8...v1.50.9) (2021-12-06)




### Bug Fixes:

* undo an unnecessary `contains` change

* WIP attempt to resolve ci_string typing errors

### Improvements:

* catch more equivalencey cases around is_nil in sat solver

## [v1.50.8](https://github.com/ash-project/ash/compare/v1.50.7...v1.50.8) (2021-12-01)




### Bug Fixes:

* case clause error in `Query.equivalent_to?`

## [v1.50.7](https://github.com/ash-project/ash/compare/v1.50.6...v1.50.7) (2021-12-01)




### Bug Fixes:

* missing rename on refactor

* typo in `unquote`

* mark `contains` as a predicate

### Improvements:

* expose small filter helpers

* make to_simple_filter fail better, add failure option

## [v1.50.6](https://github.com/ash-project/ash/compare/v1.50.5...v1.50.6) (2021-11-26)




### Improvements:

* add `Transformer.eval/3` (for special use cases only)

## [v1.50.5](https://github.com/ash-project/ash/compare/v1.50.4...v1.50.5) (2021-11-25)




### Improvements:

* track defaults being set

## [v1.50.4](https://github.com/ash-project/ash/compare/v1.50.3...v1.50.4) (2021-11-17)




### Improvements:

* add `Ash.Query.equivalent_to/2`

## [v1.50.3](https://github.com/ash-project/ash/compare/v1.50.2...v1.50.3) (2021-11-17)




### Improvements:

* add `subset_of?` and `superset_of?` query macros

## [v1.50.2](https://github.com/ash-project/ash/compare/v1.50.1...v1.50.2) (2021-11-13)




### Bug Fixes:

* run calculations inline by default

* use Date.add when using LessThanOrEqual with date value (#281)

* cast nil input on strings

### Improvements:

* support do/else blocks in if

* support `cond`

## [v1.50.1](https://github.com/ash-project/ash/compare/v1.50.0...v1.50.1) (2021-11-09)




### Bug Fixes:

* simplify and improve allow_nil checking

## [v1.50.0](https://github.com/ash-project/ash/compare/v1.49.0...v1.50.0) (2021-11-09)
### Breaking Changes:

* breaking!: explicitly setting a value to nil on create no longer falls back to the default value



## [v1.49.0](https://github.com/ash-project/ash/compare/v1.48.0-rc.30...v1.49.0) (2021-11-03)




### Bug Fixes:

* don't ask the data layer to sort if no sort is applied

* set tenant at start of query build (#278)

### Improvements:

* still filter in cases w/o a lateral join on load

## [v1.48.0-rc.30](https://github.com/ash-project/ash/compare/v1.48.0-rc.29...v1.48.0-rc.30) (2021-11-01)




### Bug Fixes:

* set storage_type to `:uuid` for Ash.Type.UUID

### Improvements:

* `only_when_valid?` on changes

## [v1.48.0-rc.29](https://github.com/ash-project/ash/compare/v1.48.0-rc.28...v1.48.0-rc.29) (2021-10-29)




### Bug Fixes:


## [v1.48.0-rc.28](https://github.com/ash-project/ash/compare/v1.48.0-rc.27...v1.48.0-rc.28) (2021-10-29)




### Bug Fixes:

* add changes from last release that I forgot

## [v1.48.0-rc.27](https://github.com/ash-project/ash/compare/v1.48.0-rc.26...v1.48.0-rc.27) (2021-10-29)




### Improvements:

* compile time optimizations via configuration

## [v1.48.0-rc.26](https://github.com/ash-project/ash/compare/v1.48.0-rc.25...v1.48.0-rc.26) (2021-10-28)




### Bug Fixes:

* correctly handle errors in validate_required_belongs_to (#276)

* set actor when loading to manage belongs_to

* cast to string before concatenating

### Improvements:

* set `action` into data layer context

## [v1.48.0-rc.25](https://github.com/ash-project/ash/compare/v1.48.0-rc.24...v1.48.0-rc.25) (2021-10-25)




### Bug Fixes:

* always lateral join for many to many relationships

### Improvements:

* add `default` option for aggregates

## [v1.48.0-rc.24](https://github.com/ash-project/ash/compare/v1.48.0-rc.23...v1.48.0-rc.24) (2021-10-25)




### Bug Fixes:

* unset `load` when running calculation queries

### Improvements:

* add `allow_async?` to calculations, default to false

* add elixir evaluation step to expression calculations

* global resource preparations

## [v1.48.0-rc.23](https://github.com/ash-project/ash/compare/v1.48.0-rc.22...v1.48.0-rc.23) (2021-10-24)




### Bug Fixes:

* breaking change! disambiguating functions in keyword filter syntax

## [v1.48.0-rc.22](https://github.com/ash-project/ash/compare/v1.48.0-rc.21...v1.48.0-rc.22) (2021-10-23)




### Bug Fixes:

* use correct typespec for `Ash.Sort.parse_input/2`

## [v1.48.0-rc.21](https://github.com/ash-project/ash/compare/v1.48.0-rc.20...v1.48.0-rc.21) (2021-10-22)




### Improvements:

* add get? metadata

## [v1.48.0-rc.20](https://github.com/ash-project/ash/compare/v1.48.0-rc.19...v1.48.0-rc.20) (2021-10-21)




### Improvements:

* custom error paths for managed relationships

## [v1.48.0-rc.19](https://github.com/ash-project/ash/compare/v1.48.0-rc.18...v1.48.0-rc.19) (2021-10-20)




### Bug Fixes:

* honor `get_by` and `get_by_identity` on bang (!) interfaces

## [v1.48.0-rc.18](https://github.com/ash-project/ash/compare/v1.48.0-rc.17...v1.48.0-rc.18) (2021-10-20)




### Improvements:

* add `get_by` and `get_by_identity` to code interface

* compile time validations for managed relationships

## [v1.48.0-rc.17](https://github.com/ash-project/ash/compare/v1.48.0-rc.16...v1.48.0-rc.17) (2021-10-19)




### Bug Fixes:

* don't require primary actions if disabled

## [v1.48.0-rc.16](https://github.com/ash-project/ash/compare/v1.48.0-rc.15...v1.48.0-rc.16) (2021-10-19)




### Improvements:

* add `primary_actions?` option

## [v1.48.0-rc.15](https://github.com/ash-project/ash/compare/v1.48.0-rc.14...v1.48.0-rc.15) (2021-10-15)




### Bug Fixes:

* don't validate allow_nil in attribute casting

## [v1.48.0-rc.14](https://github.com/ash-project/ash/compare/v1.48.0-rc.13...v1.48.0-rc.14) (2021-10-13)




### Bug Fixes:

* fix code interface on resources

### Improvements:

* breaking change! api level code interface *removed*, contact me on discord if you want a way to avoid changing to resource-based interface, but otherwise

* use proper equality checking in places where we were using simple elixir equality checking

## [v1.48.0-rc.13](https://github.com/ash-project/ash/compare/v1.48.0-rc.12...v1.48.0-rc.13) (2021-10-12)




### Bug Fixes:

* honor base query still when removing filters

## [v1.48.0-rc.12](https://github.com/ash-project/ash/compare/v1.48.0-rc.11...v1.48.0-rc.12) (2021-10-11)




### Bug Fixes:

* move related field validations to resource

* remove join_attributes, which didn't do anything anyway

### Improvements:

* add resource registry validation

## [v1.48.0-rc.11](https://github.com/ash-project/ash/compare/v1.48.0-rc.10...v1.48.0-rc.11) (2021-10-09)




### Bug Fixes:

* handle errors when validation calculation constraints

* remove certain modules from avoiding recompilation

### Improvements:

* support `module_prefix` for dsl extensions

## [v1.48.0-rc.10](https://github.com/ash-project/ash/compare/v1.48.0-rc.9...v1.48.0-rc.10) (2021-10-07)




### Bug Fixes:

* remove certain modules from avoiding recompilation

## [v1.48.0-rc.9](https://github.com/ash-project/ash/compare/v1.48.0-rc.8...v1.48.0-rc.9) (2021-10-07)




### Improvements:

* deprecation!

## [v1.48.0-rc.8](https://github.com/ash-project/ash/compare/v1.48.0-rc.7...v1.48.0-rc.8) (2021-10-06)




### Bug Fixes:

* make arrays default to nil_items?: false

### Improvements:

* breaking change! don't define code interface by default

## [v1.48.0-rc.7](https://github.com/ash-project/ash/compare/v1.48.0-rc.6...v1.48.0-rc.7) (2021-09-30)




### Bug Fixes:

* ensure changeset is up to date in `after_action` hooks

* fix a case where `unwrap_or_raise!` returned the wrong value on destroy

* fix typo on lateral join checker

* set default attribute type (in case it is explicitly set to nil) on belongs_to attributes

* if an error with no message is produced, don't attempt to concat nil with a string

### Improvements:

* simpler patterns around soft destroy actions

* add `set_option/4` to transformer helpers

* add `where` option to validate that accepts a list of validations

* prevent more unnecessary lateral joins

* only issue a lateral join when required (#269)

## [v1.48.0-rc.6](https://github.com/ash-project/ash/compare/v1.48.0-rc.5...v1.48.0-rc.6) (2021-09-20)




### Bug Fixes:

* support `on` for global changes

* return proper result when input is struct

* remove File.read! from docs

### Improvements:

* skip resource action if no changes have been made

* add `changing_attributes?/1` to determine if any attributes are changing

* add global changes

## [v1.48.0-rc.5](https://github.com/ash-project/ash/compare/v1.48.0-rc.4...v1.48.0-rc.5) (2021-09-17)




### Improvements:

* upgrade docs/tooling for elixir_sense

* set docs statically

## [v1.48.0-rc.4](https://github.com/ash-project/ash/compare/v1.48.0-rc.3...v1.48.0-rc.4) (2021-09-17)




### Improvements:

* improvements for elixirsense integration

## [v1.48.0-rc.3](https://github.com/ash-project/ash/compare/v1.48.0-rc.2...v1.48.0-rc.3) (2021-09-16)




### Improvements:

* transformer/extension improvements

* add path to errors

## [v1.48.0-rc.2](https://github.com/ash-project/ash/compare/v1.48.0-rc.1...v1.48.0-rc.2) (2021-09-15)




### Bug Fixes:

* set tenant properly on create interface

* update type spec for Ash.Sort to include single atom instead of only list. (#263)

### Improvements:

* support non-endpoint pubsub adapters

## [v1.48.0-rc.1](https://github.com/ash-project/ash/compare/v1.48.0-rc.0...v1.48.0-rc.1) (2021-09-13)




### Bug Fixes:

* check action type properly in attribute validations

## [v1.48.0-rc.0](https://github.com/ash-project/ash/compare/v1.47.12...v1.48.0-rc.0) (2021-09-13)
### Breaking Changes:

* update ecto version



### Bug Fixes:

* pass constraints to sub-fields loaded in embeddable resources

* take creates into account w/ attribute_equals and attribute_does_not_equal

* set changeset in destroy authorization request

### Improvements:

* use paramaterized types under the hood

## [v1.47.12](https://github.com/ash-project/ash/compare/v1.47.11...v1.47.12) (2021-09-12)




### Bug Fixes:

* return not found in all cases on get

* don't allow get! to return nil

* don't do db filters on creation

* honor `allow_nil_input` in required validations

### Improvements:

* add `after_action` option to create/update

* add config :ash, disable_async?: true

* add `meta[:order]` option for managed relationships

## [v1.47.11](https://github.com/ash-project/ash/compare/v1.47.10...v1.47.11) (2021-08-29)




### Bug Fixes:

* hydrate metadata types

### Improvements:

* remove metadata from read actions

## [v1.47.10](https://github.com/ash-project/ash/compare/v1.47.9...v1.47.10) (2021-08-29)




### Bug Fixes:

* update to latest picosat_elixir for releases

* cast `nil` enum values properly

* set api in destroy action hooks

### Improvements:

* support action level metadata

* add `on_match: :destroy` option

* if a map is given for a list, take it's keys

* set better error paths for invalid relationships

* include api in changeset inspect

## [v1.47.9](https://github.com/ash-project/ash/compare/v1.47.8...v1.47.9) (2021-08-11)




### Bug Fixes:

* set argument defaults early

* don't add indices to non-list inputs in managed relationships

## [v1.47.8](https://github.com/ash-project/ash/compare/v1.47.7...v1.47.8) (2021-08-05)




### Bug Fixes:

* stop managed relationships from sourcing wrong data

## [v1.47.7](https://github.com/ash-project/ash/compare/v1.47.6...v1.47.7) (2021-08-04)




### Bug Fixes:

* redact fields in the resource struct as well

* allow `before_action` to manage `belongs_to` relationships

* load belongs to relationships before managing them

* don't lookup nil input

* don't look for matches for nil

* don't accept list inputs for managed belongs_to

* don't use list inputs in belongs_to managed

* remove belongs to related after action

* fix more cases where belongs_to isn't replaced

## [v1.47.6](https://github.com/ash-project/ash/compare/v1.47.5...v1.47.6) (2021-08-01)




### Bug Fixes:

* honor `on_missing:` behavior for belongs_to relationships

* properly remove old belongs_to records

## [v1.47.5](https://github.com/ash-project/ash/compare/v1.47.4...v1.47.5) (2021-07-28)




### Bug Fixes:

* set source_query tenant in lateral join

### Improvements:

* add `belongs_to` attributes *after* the others

## [v1.47.4](https://github.com/ash-project/ash/compare/v1.47.3...v1.47.4) (2021-07-25)




### Improvements:

* pull relationship paths out of functions

## [v1.47.3](https://github.com/ash-project/ash/compare/v1.47.2...v1.47.3) (2021-07-23)




### Bug Fixes:

* no need to trap exits anymore

## [v1.47.2](https://github.com/ash-project/ash/compare/v1.47.1...v1.47.2) (2021-07-23)




### Bug Fixes:

* don't match on explicitly `:exit`

## [v1.47.1](https://github.com/ash-project/ash/compare/v1.47.0...v1.47.1) (2021-07-23)




### Bug Fixes:

* catch normal exit message from engine

* flush engine state always

## [v1.47.0](https://github.com/ash-project/ash/compare/v1.46.13...v1.47.0) (2021-07-22)




### Features:

* Ash.Resource.Info: add &public_field/2 helper (#254)

* Ash.Resource.Info: add &sortable?/3 helper

### Bug Fixes:

* load calculations from sorts properly

* rename conflicting test name

* fix complex attribute check

* disallow aggregate/calculation sorting w/ keyset pagination

## [v1.46.13](https://github.com/ash-project/ash/compare/v1.46.12...v1.46.13) (2021-07-21)




### Bug Fixes:

* ensure calculation compiled

* don't limit/offset aggregate queries

## [v1.46.12](https://github.com/ash-project/ash/compare/v1.46.11...v1.46.12) (2021-07-20)




### Bug Fixes:

* call `get_type` in cast_stored again

## [v1.46.11](https://github.com/ash-project/ash/compare/v1.46.10...v1.46.11) (2021-07-19)




### Improvements:

* speed up type loading

* add `__order__` field to be used by data layers

## [v1.46.10](https://github.com/ash-project/ash/compare/v1.46.9...v1.46.10) (2021-07-18)




### Bug Fixes:

* missing apply_attributes clause

### Improvements:

* add `force?` option to `apply_attributes/2`

## [v1.46.9](https://github.com/ash-project/ash/compare/v1.46.8...v1.46.9) (2021-07-18)




### Bug Fixes:

* demonitor engine pid after run

## [v1.46.8](https://github.com/ash-project/ash/compare/v1.46.7...v1.46.8) (2021-07-18)




### Bug Fixes:

* on_lookup read is always on destination

## [v1.46.7](https://github.com/ash-project/ash/compare/v1.46.6...v1.46.7) (2021-07-17)




### Bug Fixes:

* set item constraints properly

### Improvements:

* add on_lookup_read_action

## [v1.46.6](https://github.com/ash-project/ash/compare/v1.46.5...v1.46.6) (2021-07-15)




### Bug Fixes:

* fix simple data layer filtering

### Improvements:

* add in error paths for managed relationships

* set error paths on managed rels

## [v1.46.5](https://github.com/ash-project/ash/compare/v1.46.4...v1.46.5) (2021-07-09)




### Improvements:

* always replace error message vars

* minimize relationship source changeset context inspect size

## [v1.46.4](https://github.com/ash-project/ash/compare/v1.46.3...v1.46.4) (2021-07-08)




### Bug Fixes:

* cast empty string to nil in atom

* accept strings for atom types again

* don't turn strings to atoms in `:atom` type

* don't do unnecessary validation in type

## [v1.46.3](https://github.com/ash-project/ash/compare/v1.46.2...v1.46.3) (2021-07-05)




### Bug Fixes:

* build aggregate paths properly

## [v1.46.2](https://github.com/ash-project/ash/compare/v1.46.1...v1.46.2) (2021-07-04)




### Improvements:

* info only `required?` flag for `has_one`

## [v1.46.1](https://github.com/ash-project/ash/compare/v1.46.0...v1.46.1) (2021-07-02)




### Bug Fixes:

* properly determine reverse aggregate relationship

* ensure calculation modules are compiled

* attempt to fix calculation compile time issues

## [v1.46.0](https://github.com/ash-project/ash/compare/v1.45.0-rc20...v1.46.0) (2021-07-02)




## [v1.45.0-rc20](https://github.com/ash-project/ash/compare/v1.45.0-rc19...v1.45.0-rc20) (2021-07-01)




### Bug Fixes:

* allow sorting on aggs, w/o loading

* ensure query in `ensure_selected`

* handle sorting empty data properly

* manage ets tables properly

* link request handler to engine and runner, solve mem leak

* ensure ci_strings casted after constraints

* Revert "more optimized types"

* revert a suboptimal change to the type system

* don't call `type()` on `:string`

* optimize ash type loading

### Improvements:

* if "" fails to cast, cast it as `nil` instead

* ReadActionRequiresActor error

* `ensure_selected` change

* don't perform `match` on `nil`

* add `{:arg, :name}` input for `set_attribute`

* revamp ci_string

* add `Ash.Type.type/1`

* more optimized types

## [v1.45.0-rc19](https://github.com/ash-project/ash/compare/v1.45.0-rc18...v1.45.0-rc19) (2021-06-29)




### Bug Fixes:

* ensure query in `ensure_selected`

* handle sorting empty data properly

* manage ets tables properly

* link request handler to engine and runner, solve mem leak

* ensure ci_strings casted after constraints

* Revert "more optimized types"

* revert a suboptimal change to the type system

* don't call `type()` on `:string`

* optimize ash type loading

### Improvements:

* if "" fails to cast, cast it as `nil` instead

* ReadActionRequiresActor error

* `ensure_selected` change

* don't perform `match` on `nil`

* add `{:arg, :name}` input for `set_attribute`

* revamp ci_string

* add `Ash.Type.type/1`

* more optimized types

## [v1.45.0-rc18](https://github.com/ash-project/ash/compare/v1.45.0-rc17...v1.45.0-rc18) (2021-06-28)




### Bug Fixes:

* handle sorting empty data properly

* manage ets tables properly

* link request handler to engine and runner, solve mem leak

* ensure ci_strings casted after constraints

* Revert "more optimized types"

* revert a suboptimal change to the type system

* don't call `type()` on `:string`

* optimize ash type loading

### Improvements:

* if "" fails to cast, cast it as `nil` instead

* ReadActionRequiresActor error

* `ensure_selected` change

* don't perform `match` on `nil`

* add `{:arg, :name}` input for `set_attribute`

* revamp ci_string

* add `Ash.Type.type/1`

* more optimized types

## [v1.45.0-rc17](https://github.com/ash-project/ash/compare/v1.45.0-rc16...v1.45.0-rc17) (2021-06-28)




### Bug Fixes:

* link request handler to engine and runner, solve mem leak

* ensure ci_strings casted after constraints

* Revert "more optimized types"

* revert a suboptimal change to the type system

* don't call `type()` on `:string`

* optimize ash type loading

### Improvements:

* if "" fails to cast, cast it as `nil` instead

* ReadActionRequiresActor error

* `ensure_selected` change

* don't perform `match` on `nil`

* add `{:arg, :name}` input for `set_attribute`

* revamp ci_string

* add `Ash.Type.type/1`

* more optimized types

## [v1.45.0-rc16](https://github.com/ash-project/ash/compare/v1.45.0-rc15...v1.45.0-rc16) (2021-06-25)




### Bug Fixes:

* ensure ci_strings casted after constraints

* Revert "more optimized types"

* revert a suboptimal change to the type system

* don't call `type()` on `:string`

* optimize ash type loading

### Improvements:

* add `{:arg, :name}` input for `set_attribute`

* revamp ci_string

* add `Ash.Type.type/1`

* more optimized types

## [v1.45.0-rc15](https://github.com/ash-project/ash/compare/v1.45.0-rc14...v1.45.0-rc15) (2021-06-25)




### Bug Fixes:

* ensure ci_strings casted after constraints

* Revert "more optimized types"

* revert a suboptimal change to the type system

* don't call `type()` on `:string`

* optimize ash type loading

### Improvements:

* revamp ci_string

* add `Ash.Type.type/1`

* more optimized types

## [v1.45.0-rc14](https://github.com/ash-project/ash/compare/v1.45.0-rc13...v1.45.0-rc14) (2021-06-24)




### Bug Fixes:

* Revert "more optimized types"

* revert a suboptimal change to the type system

* don't call `type()` on `:string`

* optimize ash type loading

### Improvements:

* add `Ash.Type.type/1`

* more optimized types

## [v1.45.0-rc11](https://github.com/ash-project/ash/compare/v1.45.0-rc10...v1.45.0-rc11) (2021-06-24)




### Bug Fixes:

* revert a suboptimal change to the type system

* don't call `type()` on `:string`

* optimize ash type loading

### Improvements:

* add `Ash.Type.type/1`

* more optimized types

## [v1.45.0-rc10](https://github.com/ash-project/ash/compare/v1.45.0-rc9...v1.45.0-rc10) (2021-06-24)




### Bug Fixes:

* optimize ash type loading

## [v1.45.0-rc9](https://github.com/ash-project/ash/compare/v1.45.0-rc8...v1.45.0-rc9) (2021-06-23)




### Bug Fixes:

* pass opts through to Jason.Encode.string/2

### Improvements:

* add `error_handler` for create/update/destroy actions

## [v1.45.0-rc8](https://github.com/ash-project/ash/compare/v1.45.0-rc7...v1.45.0-rc8) (2021-06-23)




### Bug Fixes:

* properly encode ci string to json

### Improvements:

* more engine logging

## [v1.45.0-rc7](https://github.com/ash-project/ash/compare/v1.45.0-rc6...v1.45.0-rc7) (2021-06-22)




### Bug Fixes:

* `:infinity` on engine genserver calls

## [v1.45.0-rc6](https://github.com/ash-project/ash/compare/v1.45.0-rc5...v1.45.0-rc6) (2021-06-08)




### Bug Fixes:

* hide __metadata__ field on inspect

* load relationships required for calculations (optimize later)

## [v1.45.0-rc5](https://github.com/ash-project/ash/compare/v1.45.0-rc4...v1.45.0-rc5) (2021-06-08)




### Bug Fixes:

* don't halt on request handler failure

* properly error when more than 2 requests of the same type are primary

* properly process sort when the sort is an atom

* properly calculate reverse relationship paths

## [v1.45.0-rc4](https://github.com/ash-project/ash/compare/v1.45.0-rc3...v1.45.0-rc4) (2021-06-05)




### Improvements:

* support calculation sorts

## [v1.45.0-rc3](https://github.com/ash-project/ash/compare/v1.45.0-rc2...v1.45.0-rc3) (2021-06-05)




### Bug Fixes:

* always wait on the engine if it hasn't completed

## [v1.45.0-rc2](https://github.com/ash-project/ash/compare/v1.45.0-rc1...v1.45.0-rc2) (2021-06-04)




### Bug Fixes:

* always wait for engine

### Improvements:

* spawn async requests where possible

## [v1.45.0-rc1](https://github.com/ash-project/ash/compare/v1.45.0-rc0...v1.45.0-rc1) (2021-06-04)




### Bug Fixes:

* constraints on calculations

* better calculation inspect

* allow supplying stacktraces when building errors

## [v1.45.0-rc0](https://github.com/ash-project/ash/compare/v1.44.13...v1.45.0-rc0) (2021-06-04)




### Features:

* expression based calculations for filterable/sortable calculations

* expression calculations for sorting/filtering

* add compare validator (#242)

### Bug Fixes:

* aggregate authorization issues

* ensure create functions generated by code_interface accept tenant in the opts list (#243)

### Improvements:

* tons of improvements across the board

* small improvements/fixes across the board

* Update remaining builtin validators (#244)

## [v1.44.13](https://github.com/ash-project/ash/compare/v1.44.12...v1.44.13) (2021-05-28)




### Bug Fixes:

* properly process managed belongs to relationships

* cast nil -> {:ok, nil} in all cases

* set tenant even on non-tenant resources where relevant (#241)

## [v1.44.12](https://github.com/ash-project/ash/compare/v1.44.11...v1.44.12) (2021-05-23)




### Improvements:

* define embedded schemas where appropriate

## [v1.44.11](https://github.com/ash-project/ash/compare/v1.44.10...v1.44.11) (2021-05-20)




### Improvements:

* calculation.select/2 + `select` calculation option

## [v1.44.10](https://github.com/ash-project/ash/compare/v1.44.9...v1.44.10) (2021-05-20)




### Bug Fixes:

* run after_action hooks in the proper order

### Improvements:

* add `validate_destination_attribute?`

* add builtin `select` change

## [v1.44.9](https://github.com/ash-project/ash/compare/v1.44.8...v1.44.9) (2021-05-20)




### Improvements:

* set moduledoc to description if one is not set

## [v1.44.8](https://github.com/ash-project/ash/compare/v1.44.7...v1.44.8) (2021-05-19)




### Bug Fixes:

* fix dialyzer errors for enum + code interface

## [v1.44.7](https://github.com/ash-project/ash/compare/v1.44.6...v1.44.7) (2021-05-19)




### Bug Fixes:

* fix code interface + `args` combo error

### Improvements:

* support specifying the `upsert_identity` option

## [v1.44.6](https://github.com/ash-project/ash/compare/v1.44.5...v1.44.6) (2021-05-18)




### Bug Fixes:

* `ignore?: true` still accumulates changes

* properly require_attributes before setting defaults

## [v1.44.5](https://github.com/ash-project/ash/compare/v1.44.4...v1.44.5) (2021-05-18)




### Bug Fixes:

* set default values before calling resource changes

### Improvements:

* `ignore?` option for `manage_relationship` change

## [v1.44.4](https://github.com/ash-project/ash/compare/v1.44.3...v1.44.4) (2021-05-17)




### Bug Fixes:

* reverse-reverse relationship detection

## [v1.44.3](https://github.com/ash-project/ash/compare/v1.44.2...v1.44.3) (2021-05-17)




### Bug Fixes:

* support non-predicates in satsolver

## [v1.44.2](https://github.com/ash-project/ash/compare/v1.44.1...v1.44.2) (2021-05-15)




### Bug Fixes:

* raise better error w/ invalid filter expression

### Improvements:

* expose `default_value/1` in aggregate

* transactions for reads, notifications from read callbacks

## [v1.44.1](https://github.com/ash-project/ash/compare/v1.44.0...v1.44.1) (2021-05-14)




### Bug Fixes:

* handle error return in code_interface getter

## [v1.44.0](https://github.com/ash-project/ash/compare/v1.43.12...v1.44.0) (2021-05-14)




### Features:

* `on_no_match: :match` supported for to_one rels

### Bug Fixes:

* rename `context` -> `relationship_context` to avoid conflict

### Improvements:

* various managed relationship improvements

## [v1.43.12](https://github.com/ash-project/ash/compare/v1.43.11...v1.43.12) (2021-05-11)




### Improvements:

* set `__source__` context for embeds

* utility `manage_relationship_source` context

## [v1.43.11](https://github.com/ash-project/ash/compare/v1.43.10...v1.43.11) (2021-05-09)




### Bug Fixes:

* internal rename (requires version bump for other packages)

## [v1.43.10](https://github.com/ash-project/ash/compare/v1.43.9...v1.43.10) (2021-05-09)




### Bug Fixes:

* run action changes on destroy

* pattern match manage_relationship notifications fix

### Improvements:

* add `manual?` option for create/update/destroy

* ensure data layer can perform aggregates

## [v1.43.9](https://github.com/ash-project/ash/compare/v1.43.8...v1.43.9) (2021-05-09)




### Bug Fixes:

* don't fail on nil root filters

### Improvements:

* support filtering on related aggregates

* autoload aggregates used in filters

## [v1.43.8](https://github.com/ash-project/ash/compare/v1.43.7...v1.43.8) (2021-05-07)




### Bug Fixes:

* don't raise unnecessary side load error

## [v1.43.7](https://github.com/ash-project/ash/compare/v1.43.6...v1.43.7) (2021-05-07)




### Improvements:

* fix data-based side loads

## [v1.43.6](https://github.com/ash-project/ash/compare/v1.43.5...v1.43.6) (2021-05-07)




### Bug Fixes:

* IsNil function to IsNil operator

* function clause match error in not expression

## [v1.43.5](https://github.com/ash-project/ash/compare/v1.43.4...v1.43.5) (2021-05-07)




### Bug Fixes:

* limit 1 on to one side load queries

## [v1.43.4](https://github.com/ash-project/ash/compare/v1.43.3...v1.43.4) (2021-05-07)




### Improvements:

* support sorted relationships

## [v1.43.3](https://github.com/ash-project/ash/compare/v1.43.2...v1.43.3) (2021-05-06)




### Bug Fixes:

* don't consider contextual relationships as reverse relationships

* support `not` in query expressions

## [v1.43.2](https://github.com/ash-project/ash/compare/v1.43.1...v1.43.2) (2021-05-04)




### Bug Fixes:

* include aggregates in count request for aggregate filters

## [v1.43.1](https://github.com/ash-project/ash/compare/v1.43.0...v1.43.1) (2021-05-04)




### Bug Fixes:

* use base_query for aritifical limit/offset when loading

## [v1.43.0](https://github.com/ash-project/ash/compare/v1.42.0...v1.43.0) (2021-05-03)




### Features:

* rework lateral joins for many to many performance boost

### Improvements:

* add `read_action` option

## [v1.42.0](https://github.com/ash-project/ash/compare/v1.41.12...v1.42.0) (2021-04-29)




### Features:

* inner lateral join for many to many relationships

### Improvements:

* inner later join for many to many relationships

* support relationship filters

## [v1.41.12](https://github.com/ash-project/ash/compare/v1.41.11...v1.41.12) (2021-04-27)




### Improvements:

* add `has` filter predicate

## [v1.41.11](https://github.com/ash-project/ash/compare/v1.41.10...v1.41.11) (2021-04-26)




### Improvements:

* add `:list` aggregate kind

## [v1.41.10](https://github.com/ash-project/ash/compare/v1.41.9...v1.41.10) (2021-04-25)




### Bug Fixes:

* don't include `NotLoaded` in `manage_relationship`

## [v1.41.9](https://github.com/ash-project/ash/compare/v1.41.8...v1.41.9) (2021-04-23)




### Bug Fixes:

* compile time fixes

### Improvements:

* add `require_attributes` to create/update/destroy

## [v1.41.8](https://github.com/ash-project/ash/compare/v1.41.7...v1.41.8) (2021-04-21)




### Bug Fixes:

* don't consider nils for pkey matching

### Improvements:

* add first class support for enum types

* Add detailed parameter checking for Api read functions (#229)

## [v1.41.7](https://github.com/ash-project/ash/compare/v1.41.6...v1.41.7) (2021-04-18)




### Bug Fixes:

* clearer errors when resource fails to compile

* don't preload multiplicatively

* Decimal casting issues on ash_postgres (#227)

### Improvements:

* add list access to `context`

* add Resource.input/1

## [v1.41.6](https://github.com/ash-project/ash/compare/v1.41.5...v1.41.6) (2021-04-16)




### Bug Fixes:

* use items for single constraints

## [v1.41.5](https://github.com/ash-project/ash/compare/v1.41.4...v1.41.5) (2021-04-15)




### Bug Fixes:

* don't overwrite select in side_load

## [v1.41.4](https://github.com/ash-project/ash/compare/v1.41.3...v1.41.4) (2021-04-15)




### Bug Fixes:

* load relationships for management properly

* `fetch_key` bug in embedded types

* handle_indexed_maps for embedded types

## [v1.41.3](https://github.com/ash-project/ash/compare/v1.41.2...v1.41.3) (2021-04-14)




### Bug Fixes:

* handle no key provided to `NotFound`

## [v1.41.2](https://github.com/ash-project/ash/compare/v1.41.1...v1.41.2) (2021-04-13)




### Bug Fixes:

* embedded cast_stored must cast all key/values

* ci_string constraints when `nil`

* manage_relationship change turns embedded resources to maps

* fixes for common types parsing from embedded, e.g utc_datetime_usec

### Improvements:

* special provisions for casting to embedded type (e.g uuid)

## [v1.41.1](https://github.com/ash-project/ash/compare/v1.41.0...v1.41.1) (2021-04-13)




### Bug Fixes:

* `get!` should raise on `nil` not `{:ok, nil}`

## [v1.41.0](https://github.com/ash-project/ash/compare/v1.40.0...v1.41.0) (2021-04-13)




### Features:

* change `get?: true` interface functions to raise on `nil`

### Bug Fixes:

* allow_nil -> allow_nil_input

* allow api.load/2 to load calculations

### Improvements:

* add `allow_nil_input` to create actions for api layers

* add `load/1` builtin change

## [v1.40.0](https://github.com/ash-project/ash/compare/v1.39.7...v1.40.0) (2021-04-13)




### Features:

* change `get?: true` interface functions to raise on `nil`

### Bug Fixes:

* allow api.load/2 to load calculations

### Improvements:

* add `allow_nil_input` to create actions for api layers

* add `load/1` builtin change

## [v1.39.7](https://github.com/ash-project/ash/compare/v1.39.6...v1.39.7) (2021-04-12)




### Bug Fixes:

* always select necessary load fields for nested loads

## [v1.39.6](https://github.com/ash-project/ash/compare/v1.39.5...v1.39.6) (2021-04-10)




### Bug Fixes:

* always select necessary fields for side loading

## [v1.39.5](https://github.com/ash-project/ash/compare/v1.39.4...v1.39.5) (2021-04-09)




### Bug Fixes:

* logic bug in selecting specific fields

## [v1.39.4](https://github.com/ash-project/ash/compare/v1.39.3...v1.39.4) (2021-04-09)




### Improvements:

* support the datalayer selecting fields in reads

## [v1.39.3](https://github.com/ash-project/ash/compare/v1.39.2...v1.39.3) (2021-04-04)




### Improvements:

* add sum aggregate (#221)

## [v1.39.2](https://github.com/ash-project/ash/compare/v1.39.1...v1.39.2) (2021-04-04)




### Improvements:

* allow specifying that calculation can't be nil (#220)

## [v1.39.1](https://github.com/ash-project/ash/compare/v1.39.0...v1.39.1) (2021-04-03)




### Bug Fixes:

* update struct_field logic for latest ecto

* apply proper interface operation when opts aren't passed

## [v1.39.0](https://github.com/ash-project/ash/compare/v1.38.0...v1.39.0) (2021-04-01)




### Features:

* support `Ash.Query.distinct/2`

* add `build/2` query preparation

### Bug Fixes:

* manage_relationships *before* after_action callbacks

### Improvements:

* `before_action?` on `validate`, validate inline

## [v1.38.0](https://github.com/ash-project/ash/compare/v1.37.2...v1.38.0) (2021-03-31)




### Features:

* support `Ash.Query.distinct/2`

* add `build/2` query preparation

### Bug Fixes:

* manage_relationships *before* after_action callbacks

## [v1.37.2](https://github.com/ash-project/ash/compare/v1.37.1...v1.37.2) (2021-03-29)




### Bug Fixes:

* don't overwrite managed `belongs_to` relationships

* handle `on_lookup` + `on_no_match` for `belongs_to`

* fix required relationships and add test

* fix required relationships

* various managed_relationship fixes

## [v1.37.1](https://github.com/ash-project/ash/compare/v1.37.0...v1.37.1) (2021-03-28)




### Bug Fixes:

* fix required relationships and add test

* fix required relationships

* various managed_relationship fixes

## [v1.37.0](https://github.com/ash-project/ash/compare/v1.36.22...v1.37.0) (2021-03-25)




### Features:

* add manage relationship types

### Improvements:

* don't accept relationships on actions anymore

* require arguments

## [v1.36.22](https://github.com/ash-project/ash/compare/v1.36.21...v1.36.22) (2021-03-24)




### Bug Fixes:

* add tenant metadata before after action hooks

## [v1.36.21](https://github.com/ash-project/ash/compare/v1.36.20...v1.36.21) (2021-03-24)




### Bug Fixes:

* support type aliases in more type casting functions

* support `tenant` option in read interface

## [v1.36.20](https://github.com/ash-project/ash/compare/v1.36.19...v1.36.20) (2021-03-24)




### Bug Fixes:

* support `tenant` option in read interface

## [v1.36.19](https://github.com/ash-project/ash/compare/v1.36.18...v1.36.19) (2021-03-22)




### Bug Fixes:

* always return changeset when runner failed

## [v1.36.18](https://github.com/ash-project/ash/compare/v1.36.17...v1.36.18) (2021-03-22)




### Improvements:

* docs + dialyzer + error improvements

## [v1.36.17](https://github.com/ash-project/ash/compare/v1.36.16...v1.36.17) (2021-03-22)




### Bug Fixes:

* don't require values when managing relationships

* Revert "fix: force_change_attributes before passing to action"

## [v1.36.16](https://github.com/ash-project/ash/compare/v1.36.15...v1.36.16) (2021-03-22)




### Bug Fixes:

* force_change_attributes before passing to action

## [v1.36.15](https://github.com/ash-project/ash/compare/v1.36.14...v1.36.15) (2021-03-21)




### Bug Fixes:

* exception on invalid query arguments

* allow casting strings as uuids (for embedded types)

### Improvements:

* retain actor context from changeset

## [v1.36.14](https://github.com/ash-project/ash/compare/v1.36.13...v1.36.14) (2021-03-21)




### Improvements:

* Add float type (#204)

## [v1.36.13](https://github.com/ash-project/ash/compare/v1.36.12...v1.36.13) (2021-03-20)




### Bug Fixes:

* avoid exception in `Changeset.new/2` for bad attribute

* use ecto's uuid type under the hood

### Improvements:

* raise informative errors on bad inputs to `for_*`

## [v1.36.12](https://github.com/ash-project/ash/compare/v1.36.11...v1.36.12) (2021-03-19)




### Improvements:

* make `Ash.Error` a public module

## [v1.36.11](https://github.com/ash-project/ash/compare/v1.36.10...v1.36.11) (2021-03-19)




### Improvements:

* docs/default value for params

## [v1.36.10](https://github.com/ash-project/ash/compare/v1.36.9...v1.36.10) (2021-03-19)




### Bug Fixes:

* set `source_attribute` when replacing `belongs_to` relationship

* don't consider `false` as absent value

* set argument name in `manage_relationship`

### Improvements:

* trim whitespace in uuid

## [v1.36.9](https://github.com/ash-project/ash/compare/v1.36.8...v1.36.9) (2021-03-18)




### Improvements:

* improve the behavior of defaults

## [v1.36.8](https://github.com/ash-project/ash/compare/v1.36.7...v1.36.8) (2021-03-17)




### Bug Fixes:

* validate required attributes *after* before_action hooks

## [v1.36.7](https://github.com/ash-project/ash/compare/v1.36.6...v1.36.7) (2021-03-17)




### Improvements:

* discard certain empty values for embed input

## [v1.36.6](https://github.com/ash-project/ash/compare/v1.36.5...v1.36.6) (2021-03-15)




### Bug Fixes:

* force_change attrs *after* `for_create/update`

* pattern match errors in `manage_relationships`

* clean up some error cases

* only default accept to `public` attributes

* allow_nil?: false + default interaction

## [v1.36.5](https://github.com/ash-project/ash/compare/v1.36.4...v1.36.5) (2021-03-14)




### Bug Fixes:

* remove the `as` option

### Improvements:

* Add timestamps() attribute (#198)

## [v1.36.4](https://github.com/ash-project/ash/compare/v1.36.3...v1.36.4) (2021-03-13)




### Bug Fixes:

* properly validate `allow_nil?: false` on update

* properly validate `allow_nil?: false` private attributes

## [v1.36.3](https://github.com/ash-project/ash/compare/v1.36.2...v1.36.3) (2021-03-13)




### Bug Fixes:

* set argument default on cast

## [v1.36.2](https://github.com/ash-project/ash/compare/v1.36.1...v1.36.2) (2021-03-12)




### Bug Fixes:

* fix pub_sub on update

* fix `publish_all` pub_sub notifier

### Improvements:

* derive has_one destination_attribute

* finalize code API logic

* add not_found_message + violation_message for relationships

* support `get_by_<identity>` in interface

* support sublists in pub_sub topics

* support `:_tenant` in pub_sub topics

## [v1.36.1](https://github.com/ash-project/ash/compare/v1.36.0...v1.36.1) (2021-03-09)




### Bug Fixes:

* properly filter aggregates

### Improvements:

* accept `tenant` in `for_read`

## [v1.36.0](https://github.com/ash-project/ash/compare/v1.35.1...v1.36.0) (2021-03-08)




### Features:

* functional interface on the Api module

* resource aliases

### Improvements:

* update interface to accept query/changesets

* require completely unique action names

## [v1.35.1](https://github.com/ash-project/ash/compare/v1.35.0...v1.35.1) (2021-03-07)




### Bug Fixes:

* don't reverse sub-entities in DSL

## [v1.35.0](https://github.com/ash-project/ash/compare/v1.34.9...v1.35.0) (2021-03-07)




### Features:

* support `Ash.Query.select/3` and `Ash.Changeset.select/3`

## [v1.34.9](https://github.com/ash-project/ash/compare/v1.34.8...v1.34.9) (2021-03-05)




### Improvements:

* ignore destination field on some relationship inputs

## [v1.34.8](https://github.com/ash-project/ash/compare/v1.34.7...v1.34.8) (2021-03-05)




### Bug Fixes:

* various validation lifecycle fixes

* don't fetch sideloads for empty data

### Improvements:

* various validation lifecycle options

## [v1.34.7](https://github.com/ash-project/ash/compare/v1.34.6...v1.34.7) (2021-02-26)




### Bug Fixes:

* fix nested boolean expression optimization

## [v1.34.6](https://github.com/ash-project/ash/compare/v1.34.5...v1.34.6) (2021-02-24)




### Bug Fixes:

* manage_relationship fixes, input + option defaults

## [v1.34.5](https://github.com/ash-project/ash/compare/v1.34.4...v1.34.5) (2021-02-24)




### Bug Fixes:

* treat empty string as `nil` in `manage_relationship`

* be more conservative (and more correct) when optimizing predicates

## [v1.34.4](https://github.com/ash-project/ash/compare/v1.34.3...v1.34.4) (2021-02-24)




### Bug Fixes:

* treat empty string as `nil` in `manage_relationship`

* be more conservative (and more correct) when optimizing predicates

## [v1.34.3](https://github.com/ash-project/ash/compare/v1.34.2...v1.34.3) (2021-02-23)




### Bug Fixes:

* fix builtin `mange_relationship` change

## [v1.34.2](https://github.com/ash-project/ash/compare/v1.34.1...v1.34.2) (2021-02-23)




### Bug Fixes:

* support belongs_to relationships properly

## [v1.34.1](https://github.com/ash-project/ash/compare/v1.34.0...v1.34.1) (2021-02-23)




### Bug Fixes:

* authorize if actor key is present

## [v1.34.0](https://github.com/ash-project/ash/compare/v1.33.1...v1.34.0) (2021-02-23)




### Features:

* refactored manage_relationship options/behavior

### Improvements:

* many compile time fixes via code splitting

* Guess destination_attribute for has many relationships (#187)

* Implement string length validation (#183)

## [v1.33.1](https://github.com/ash-project/ash/compare/v1.33.0...v1.33.1) (2021-02-23)




### Improvements:

* many compile time fixes via code splitting

* Guess destination_attribute for has many relationships (#187)

* Implement string length validation (#183)

## [v1.33.0](https://github.com/ash-project/ash/compare/v1.32.2...v1.33.0) (2021-02-05)




### Features:

* add default_context

* add `manage_relationship/4`

* add relationship specific context (for postgres polymorphism)

* add `reject` (opposite of `accept`)

### Bug Fixes:

* support `manage_relationship` for `belongs_to`

### Improvements:

* set_context change/preparation

* set `accept` by default

## [v1.32.2](https://github.com/ash-project/ash/compare/v1.32.1...v1.32.2) (2021-01-28)




### Improvements:

* support `{:filter, _}` authorization results for changesets

## [v1.32.1](https://github.com/ash-project/ash/compare/v1.32.0...v1.32.1) (2021-01-27)




### Bug Fixes:

* only run authorization once per request

* don't error on replacing empty relationship with empty

### Improvements:

* support `tenant` option to `get/2`

* support `message` option on identities

## [v1.32.0](https://github.com/ash-project/ash/compare/v1.31.1...v1.32.0) (2021-01-25)




### Features:

* add `after_action` for queries

### Bug Fixes:

* default to calculating filters on `data_layer_query`

## [v1.31.1](https://github.com/ash-project/ash/compare/v1.31.0...v1.31.1) (2021-01-24)




### Bug Fixes:

* remove invalid boolean expression optimization

### Improvements:

* make form errors work better with phoenix

## [v1.31.0](https://github.com/ash-project/ash/compare/v1.30.2...v1.31.0) (2021-01-24)




### Features:

* add `contains/2` query function

### Bug Fixes:

* various ci_string improvements

## [v1.30.2](https://github.com/ash-project/ash/compare/v1.30.1...v1.30.2) (2021-01-22)




### Bug Fixes:

* add explicit jason dependency

## [v1.30.1](https://github.com/ash-project/ash/compare/v1.30.0...v1.30.1) (2021-01-22)




### Bug Fixes:

* update elixir versions in CI

## [v1.30.0](https://github.com/ash-project/ash/compare/v1.29.0-rc1...v1.30.0) (2021-01-22)




### Bug Fixes:

* add action filters in `for_read/3`

* don't let local runner processes mix up messages

* runtime filter filters properly

## [v1.29.0-rc1](https://github.com/ash-project/ash/compare/v1.28.0-rc0...v1.29.0-rc1) (2021-01-21)




## [v1.29.0-rc0](https://github.com/ash-project/ash/compare/v1.28.1...v1.29.0-rc0) (2021-01-21)




### Features:

* freeform expressions

* validatiosn in actions

* query arguments

* add `Ash.Query.for_read/3`

* return changeset with API errors

* add case insensitive string `CiString`/`:ci_string`

* support `context/1` and `arg/1` in filter templates

* support targeting notifications with the `for` option

* add `ago/2` query function

* add basic arithmetic operators (+, *, -, /)

* `sensitive?` option for attributes

* `sensitive?` option for arguments

* `private` arguments, which canât be set using `for_<action>`

* add `prevent_change` which will erase changes just before the changeset is committed

* add `match?` validation that supports a custom error message

* add `interval` type to support `ago/2` function

* add `url_encoded_binary` type

* add `function` type

### Bug Fixes:

* properly expand module aliases for options w/o compile time dependency

### Improvements:

* support all string constraints for ci_string

* `changing?` is now a validation

* add `Transformer.get_persisted/3`

* add `api` field to `Notification`

* standardize errors, add `to_error_class`

* use `Comp` everywhere

* use action on changeset if set by `for_<action_type>`

* `action_failed?` field on change sets

* remove ability for data layers to add operators (for now at least)

* Changeset.apply_attributes/2 now returns an error tuple

* add a bunch of new/informative errors

* runtime filter now uses left join logic (a naive implementation of it)

* support more filter templates in resources

* basic/naive type system for operators/functions

* Add trim/allow_empty to string type (#171)

## [v1.28.1](https://github.com/ash-project/ash/compare/v1.28.0...v1.28.1) (2021-01-12)




### Improvements:

* Improve attribute defaults (#164)

## [v1.28.0](https://github.com/ash-project/ash/compare/v1.27.1...v1.28.0) (2021-01-12)




### Features:

* Add Embedded Resources (#170)

### Bug Fixes:

* Correct error message (#163)

### Improvements:

* Add built in decimal type (#162)

## [v1.27.1](https://github.com/ash-project/ash/compare/v1.27.0...v1.27.1) (2021-01-08)




### Bug Fixes:

* fix small sort bugs

### Improvements:

* add `Ash.Sort.parse_input/2`

## [v1.27.0](https://github.com/ash-project/ash/compare/v1.26.13...v1.27.0) (2021-01-08)
### Breaking Changes:

* Use usec timestamps by default



### Improvements:

* Add built in usec datetime type (#160) (#161)

## [v1.26.13](https://github.com/ash-project/ash/compare/v1.26.12...v1.26.13) (2021-01-08)




### Bug Fixes:

* only cast public relationships/attributes

## [v1.26.12](https://github.com/ash-project/ash/compare/v1.26.11...v1.26.12) (2021-01-08)




### Bug Fixes:

* `allow_nil?: false` for `integer_primary_key`

## [v1.26.11](https://github.com/ash-project/ash/compare/v1.26.10...v1.26.11) (2021-01-08)




### Improvements:

* add `for_<action>` helpers

## [v1.26.10](https://github.com/ash-project/ash/compare/v1.26.9...v1.26.10) (2021-01-07)




### Improvements:

* Add built in binary type (#156)

## [v1.26.9](https://github.com/ash-project/ash/compare/v1.26.8...v1.26.9) (2021-01-06)




### Bug Fixes:

* the `__resource__` change broke some extensions

## [v1.26.8](https://github.com/ash-project/ash/compare/v1.26.7...v1.26.8) (2021-01-06)




### Bug Fixes:

* add back `extensions/1` helper to resources

## [v1.26.7](https://github.com/ash-project/ash/compare/v1.26.6...v1.26.7) (2021-01-06)




### Bug Fixes:

* lazy loaded module issues (e.g in iex)

### Improvements:

* optimize not-in and fix dialyzer

* rework filter creation + subset checking

## [v1.26.6](https://github.com/ash-project/ash/compare/v1.26.5...v1.26.6) (2020-12-30)




### Bug Fixes:

* validate read action existence

### Improvements:

* support autocompletion on Api funcs

## [v1.26.5](https://github.com/ash-project/ash/compare/v1.26.4...v1.26.5) (2020-12-30)




### Improvements:

* default actions

## [v1.26.4](https://github.com/ash-project/ash/compare/v1.26.3...v1.26.4) (2020-12-30)




### Bug Fixes:

* fix compile issues, add docs

## [v1.26.3](https://github.com/ash-project/ash/compare/v1.26.2...v1.26.3) (2020-12-30)




### Improvements:

* add `parse_input/3` to `Ash.Filter`

## [v1.26.2](https://github.com/ash-project/ash/compare/v1.26.1...v1.26.2) (2020-12-29)




### Improvements:

* describe operator types

## [v1.26.1](https://github.com/ash-project/ash/compare/v1.26.0...v1.26.1) (2020-12-29)




### Bug Fixes:

* only accept kw list in `aggregate/5`

## [v1.26.0](https://github.com/ash-project/ash/compare/v1.25.8...v1.26.0) (2020-12-29)




### Features:

* support `:first` aggregate (#153)

* support more sort orders

## [v1.25.8](https://github.com/ash-project/ash/compare/v1.25.7...v1.25.8) (2020-12-27)




### Bug Fixes:

* separate builders + description in sections

## [v1.25.7](https://github.com/ash-project/ash/compare/v1.25.6...v1.25.7) (2020-12-27)




### Bug Fixes:

* support `examples` on dsl sections

## [v1.25.6](https://github.com/ash-project/ash/compare/v1.25.5...v1.25.6) (2020-12-27)




### Bug Fixes:

* cast string argument names

* uuid/id pkeys should `allow_nil`

## [v1.25.5](https://github.com/ash-project/ash/compare/v1.25.4...v1.25.5) (2020-12-23)




### Bug Fixes:

* support operators on both sides for not_eq

## [v1.25.4](https://github.com/ash-project/ash/compare/v1.25.3...v1.25.4) (2020-12-23)




### Bug Fixes:

* fix filtering for ets + mnesia data layers

## [v1.25.3](https://github.com/ash-project/ash/compare/v1.25.2...v1.25.3) (2020-12-23)




### Bug Fixes:

* various pagination, runtime, and auth bugs

* default pagination limit triggers pagination

## [v1.25.2](https://github.com/ash-project/ash/compare/v1.25.1...v1.25.2) (2020-12-06)




### Bug Fixes:

* resolve warning from nimbleoptions deprecation

## [v1.25.1](https://github.com/ash-project/ash/compare/v1.25.0...v1.25.1) (2020-12-02)




### Improvements:

* support confirming arguments, test allow_nil?

## [v1.25.0](https://github.com/ash-project/ash/compare/v1.24.2...v1.25.0) (2020-12-02)




### Features:

* support arguments for actions

## [v1.24.2](https://github.com/ash-project/ash/compare/v1.24.1...v1.24.2) (2020-12-01)




### Bug Fixes:

* various build fixes

* various small utility fixes

* update get-tag

## [v1.24.1](https://github.com/ash-project/ash/compare/v1.24.0...v1.24.1) (2020-11-08)




### Bug Fixes:

* do not require private attributes in create api (#143)

## [v1.24.0](https://github.com/ash-project/ash/compare/v1.23.3...v1.24.0) (2020-11-07)




### Features:

* add uuid_primary_key/2 and integer_primary_key/2

## [v1.23.3](https://github.com/ash-project/ash/compare/v1.23.2...v1.23.3) (2020-11-07)




### Bug Fixes:

* derived belongs_to attributes are required if their parent is

## [v1.23.2](https://github.com/ash-project/ash/compare/v1.23.1...v1.23.2) (2020-11-06)




### Bug Fixes:

* default create/update timestamps to private?

## [v1.23.1](https://github.com/ash-project/ash/compare/v1.23.0...v1.23.1) (2020-11-06)




### Bug Fixes:

* set proper pagination defaults

## [v1.23.0](https://github.com/ash-project/ash/compare/v1.22.1...v1.23.0) (2020-11-03)




### Features:

* Add property: private? for attributes, relationships, aggregates, and calculations (#140)

## [v1.22.1](https://github.com/ash-project/ash/compare/v1.22.0...v1.22.1) (2020-10-29)




### Improvements:

* support specifying that some options are modules

## [v1.22.0](https://github.com/ash-project/ash/compare/v1.21.0...v1.22.0) (2020-10-28)




### Features:

* multitenancy! and tons of various fixes (#139)

## [v1.21.0](https://github.com/ash-project/ash/compare/v1.20.1...v1.21.0) (2020-10-28)



### Improvements:

* trace $callers through engine genservers

## [v1.20.1](https://github.com/ash-project/ash/compare/v1.20.0...v1.20.1) (2020-10-21)




### Bug Fixes:

* better not_found error handling

## [v1.20.0](https://github.com/ash-project/ash/compare/v1.19.1...v1.20.0) (2020-10-21)




### Features:

* Optimize relationship records replacement (#135)

### Bug Fixes:

* remove unused code

* various fixes and improvements

## [v1.19.1](https://github.com/ash-project/ash/compare/v1.19.0...v1.19.1) (2020-10-17)




### Bug Fixes:

* invalid function arg parsing w/ ref

## [v1.19.0](https://github.com/ash-project/ash/compare/v1.18.1...v1.19.0) (2020-10-17)




### Features:

* pubsub notifier (#134)

## [v1.18.1](https://github.com/ash-project/ash/compare/v1.18.0...v1.18.1) (2020-10-16)




### Bug Fixes:

* engine hanging on parallel requests

## [v1.18.0](https://github.com/ash-project/ash/compare/v1.17.1...v1.18.0) (2020-10-15)




### Features:

* add notifiers (#133)

* Add `:one_of` constraint to the Atom type (#130)

## [v1.17.1](https://github.com/ash-project/ash/compare/v1.17.0...v1.17.1) (2020-10-12)




### Bug Fixes:

* bugs with keyset pagination

## [v1.17.0](https://github.com/ash-project/ash/compare/v1.16.2...v1.17.0) (2020-10-12)




### Features:

* Add pagination (#131)

## [v1.16.2](https://github.com/ash-project/ash/compare/v1.16.1...v1.16.2) (2020-10-10)




### Bug Fixes:

* parse functions properly

## [v1.16.1](https://github.com/ash-project/ash/compare/v1.16.0...v1.16.1) (2020-10-10)




### Bug Fixes:

* fix dialyzer

* fix certain versions of elixir having issues

## [v1.16.0](https://github.com/ash-project/ash/compare/v1.15.1...v1.16.0) (2020-10-08)




### Features:

* expression based filter

## [v1.15.1](https://github.com/ash-project/ash/compare/v1.15.0...v1.15.1) (2020-10-07)




## [v1.15.0](https://github.com/ash-project/ash/compare/v1.14.0...v1.15.0) (2020-10-06)




### Features:

* filter rewrite to op/function/ref based structure

* added description for missing resources (#117)

### Bug Fixes:

* add module name to errors (#127)

* Fix composite key in changeset functions (#125)

## [v1.14.0](https://github.com/ash-project/ash/compare/v1.13.4...v1.14.0) (2020-09-24)




### Features:

* descriptions for actions and relationships (#116)

### Bug Fixes:

* typespec/error message improvements

## [v1.13.4](https://github.com/ash-project/ash/compare/v1.13.3...v1.13.4) (2020-09-21)




### Bug Fixes:

* upgrade picosat dependency

* correct comment in UUID type (#115)

## [v1.13.3](https://github.com/ash-project/ash/compare/v1.13.2...v1.13.3) (2020-09-19)



### Features:

* set_attribute builtin change

* (greater_than/less_than)_or_equal predicates

* support deletes as updates via "soft"

* support base_filters

### Bug Fixes:

* less_than predicate was flipped for runtime


## [v1.13.2](https://github.com/ash-project/ash/compare/v1.13.1...v1.13.2) (2020-09-07)




### Bug Fixes:

* remove delegate data layer (#112)

* delete process/global storage properly

## [v1.13.1](https://github.com/ash-project/ash/compare/v1.13.0...v1.13.1) (2020-09-04)




### Bug Fixes:

* Fix identities (#110)

## [v1.13.0](https://github.com/ash-project/ash/compare/v1.12.0...v1.13.0) (2020-09-02)




### Features:

* required belongs_to relationships (#107)

* support filter templates on read actions

* builtin concat calculation

* add changes to actions (#106)

* add `accept` option to create/update actions (#105)

* add `Ash.NotLoaded` back, simpler defaults

* improve errors (add stacktraces)

### Bug Fixes:

* various delegate data layer improvements

* engine halting issues

* resolve engine deadlocks

* support nested lists of filters

## [v1.12.0](https://github.com/ash-project/ash/compare/v1.11.1...v1.12.0) (2020-08-27)

### Features:

- add `one_of` validation

- add `simple` data layer, and make it default

### Bug Fixes:

- allow anonymous functions in the dsl

## [v1.11.1](https://github.com/ash-project/ash/compare/v1.11.0...v1.11.1) (2020-08-26)

### Bug Fixes:

- only update filter when its a filter

- set resource in delegation query

## [v1.11.0](https://github.com/ash-project/ash/compare/v1.10.0...v1.11.0) (2020-08-26)

### Features:

- support inner lateral joins (#100)

- add identities, to enhance `get` calls (#99)

- initial calculation support (#98)

- initial calculation support

### Bug Fixes:

- spec + dialyzer fixes

- spec mix task run as no_return

## [v1.10.0](https://github.com/ash-project/ash/compare/v1.9.0...v1.10.0) (2020-08-22)

### Features:

- Add delegate data_layer (#97)

## [v1.9.0](https://github.com/ash-project/ash/compare/v1.8.0...v1.9.0) (2020-08-19)

### Features:

- various custom data_layer features

## [v1.8.0](https://github.com/ash-project/ash/compare/v1.7.0...v1.8.0) (2020-08-18)

### Features:

- streamline `load` by accepting ok/error

### Bug Fixes:

- extensions: resolve duplicate nested entities

- use Ecto's NotLoaded for assocs for now

- create/update typespecs were wrong

## [v1.7.0](https://github.com/ash-project/ash/compare/v1.6.8...v1.7.0) (2020-08-17)

### Features:

- add `is_nil` predicate

### Bug Fixes:

- add lexical scope to DSL for imports

- crash on entity w/ no options specified

- convert `eq: nil` into is_nil, fix credo

## [v1.6.8](https://github.com/ash-project/ash/compare/v1.6.7...v1.6.8) (2020-08-15)

### Bug Fixes:

- some day tag names will work

## [v1.6.7](https://github.com/ash-project/ash/compare/v1.6.6...v1.6.7) (2020-08-15)

### Bug Fixes:

- get the tag name for real this time

## [v1.6.6](https://github.com/ash-project/ash/compare/v1.6.5...v1.6.6) (2020-08-15)

### Bug Fixes:

- try again to get the version name in tweets

## [v1.6.5](https://github.com/ash-project/ash/compare/v1.6.4...v1.6.5) (2020-08-15)

### Bug Fixes:

- get the version property

## [v1.6.4](https://github.com/ash-project/ash/compare/v1.6.3...v1.6.4) (2020-08-15)

### Bug Fixes:

- test out automatic tweeting

## [v1.6.3](https://github.com/ash-project/ash/compare/v1.6.2...v1.6.3) (2020-08-15)

### Bug Fixes:

- remove bad package reference from the docs

## [v1.6.2](https://github.com/ash-project/ash/compare/v1.6.1...v1.6.2) (2020-08-13)

### Bug Fixes:

- various fixes for graphql extension

## [v1.6.1](https://github.com/ash-project/ash/compare/v1.6.0...v1.6.1) (2020-08-10)

### Bug Fixes:

- `load` typespecs

## [v1.6.0](https://github.com/ash-project/ash/compare/v1.5.1...v1.6.0) (2020-08-10)

### Features:

- add named aggregates

### Bug Fixes:

- various fixes from json:api integration

## [v1.5.1](https://github.com/ash-project/ash/compare/v1.5.0...v1.5.1) (2020-07-24)

### Bug Fixes:

- scope data layer feature to aggregate kind

## v1.5.0 (2020-07-24)

### Features:

- group metadata by path and async?: false

- run aggregates async if not in query

- aggregations!

## [v1.4.1](https://github.com/ash-project/ash/compare/1.4.0...v1.4.1) (2020-07-20)

### Bug Fixes:

- simplify dsl building using `on_load`

## [v1.4.0](https://github.com/ash-project/ash/compare/1.3.1...v1.4.0) (2020-07-16)

### Features:

- allow editing join association attributes

## [v1.3.1](https://github.com/ash-project/ash/compare/1.3.0...v1.3.1) (2020-07-16)

### Bug Fixes:

- use proper errors everywhere

## [v1.3.0](https://github.com/ash-project/ash/compare/1.2.1...v1.3.0) (2020-07-15)

### Features:

- various small refactors + validations

## [v1.2.1](https://github.com/ash-project/ash/compare/1.2.0...v1.2.1) (2020-07-13)

### Bug Fixes:

- changeset + set_state issues

## [v1.2.0](https://github.com/ash-project/ash/compare/1.1.3...v1.2.0) (2020-07-13)

### Features:

- refactor changes into changesets

## [v1.1.3](https://github.com/ash-project/ash/compare/1.1.2...v1.1.3) (2020-07-09)

## [v1.1.2](https://github.com/ash-project/ash/compare/1.1.1...v1.1.2) (2020-07-09)

## [v1.1.1](https://github.com/ash-project/ash/compare/1.1.0...v1.1.1) (2020-07-09)

### Bug Fixes:

- small fixes

## [v1.1.0](https://github.com/ash-project/ash/compare/1.0.3...v1.1.0) (2020-07-09)

### Features:

- lots of docs, simplify query generation

- validate relationship keys

## [v1.0.3](https://github.com/ash-project/ash/compare/1.0.2...v1.0.3) (2020-07-08)

## [v1.0.2](https://github.com/ash-project/ash/compare/1.0.1...v1.0.2) (2020-07-07)

## [v1.0.1](https://github.com/ash-project/ash/compare/1.0.0...v1.0.1) (2020-07-07)

## [v1.0.0](https://github.com/ash-project/ash/compare/0.10.0...v1.0.0) (2020-07-07)

### Breaking Changes:

- remove initial subscriptions pass

### Features:

- general improvements

### Bug Fixes:

- in predicate + engine errors

## [v0.10.0](https://github.com/ash-project/ash/compare/0.9.1...v0.10.0) (2020-07-02)

### Breaking Changes:

- remove atom type, add docs

### Features:

- list types

- refactor ash types to modules, add constraints

### Bug Fixes:

- remove benchee, ensure mnesia always uses transactions

- try clearing cache to fix CI

- stop gitignoring the mnesia data layer

- try to fix ash.formatter task

- test/improve parallelizable requests

- require that resources have primary keys

## [v0.9.1](https://github.com/ash-project/ash/compare/0.9.0...v0.9.1) (2020-06-30)

### Bug Fixes:

- move to simpler transaction logic

## [v0.9.0](https://github.com/ash-project/ash/compare/0.8.0...v0.9.0) (2020-06-29)

### Features:

- add less_than and greater_than filter support

- validate all related resources in API

### Bug Fixes:

- fix tests/credo

- fix tests, add tests for gt/lt filters

## [v0.8.0](https://github.com/ash-project/ash/compare/0.7.0...v0.8.0) (2020-06-29)

### Features:

- cross data layer filters

- cross data layer filtering

## [v0.7.0](https://github.com/ash-project/ash/compare/0.6.5...v0.7.0) (2020-06-27)

### Features:

- section option configuration

### Bug Fixes:

- set persistent_term properly

## [v0.6.5](https://github.com/ash-project/ash/compare/0.6.4...v0.6.5) (2020-06-22)

### Bug Fixes:

- use authorization filters in side loads

## [v0.6.4](https://github.com/ash-project/ash/compare/0.6.3...v0.6.4) (2020-06-22)

### Bug Fixes:

- remove reverse relationships

## [v0.6.3](https://github.com/ash-project/ash/compare/0.6.2...v0.6.3) (2020-06-22)

### Bug Fixes:

- many filter/side load fixes/improvements

## [v0.6.2](https://github.com/ash-project/ash/compare/0.6.1...v0.6.2) (2020-06-20)

### Bug Fixes:

- allow side_load option on create/update

## [v0.6.1](https://github.com/ash-project/ash/compare/0.6.0...v0.6.1) (2020-06-20)

### Bug Fixes:

- raised error message contents

- parent error messages

- relationship path clause

## [v0.6.0](https://github.com/ash-project/ash/compare/0.5.2...v0.6.0) (2020-06-19)

### Features:

- boolean filter refactor (#78)

- predicate behaviour

## [v0.5.2](https://github.com/ash-project/ash/compare/0.5.1...v0.5.2) (2020-06-15)

### Bug Fixes:

- consider nested entities in ash.formatter

## [v0.5.1](https://github.com/ash-project/ash/compare/0.5.0...v0.5.1) (2020-06-15)

### Bug Fixes:

- compile application in ash.formatter task

## [v0.5.0](https://github.com/ash-project/ash/compare/0.4.0...v0.5.0) (2020-06-15)

### Features:

- extension section module imports, generated .formatter.exs (#71)

## [v0.4.0](https://github.com/ash-project/ash/compare/0.3.0...v0.4.0) (2020-06-14)

### Features:

- rebuild DSL inner workings for extensibility (#70)

- add `after_compile` and validate primary key

### Bug Fixes:

- dialyzer warnings

- honor the `authorize?` flag

### Improvements:

- add `date` support (#68)

## [v0.3.0](https://github.com/ash-project/ash/compare/0.2.0...v0.3.0) (2020-06-05)

### Features:

- remove name/type from ash core

### Bug Fixes:

- account for action/actor args to interface

- remove the rest of the deps on name/type

- add `resource_module?/1` back to `Ash`

## [v0.2.0](https://github.com/ash-project/ash/compare/0.1.9...v0.2.0) (2020-06-05)

### Features:

- use option schemas in the interface (#30)

## [v0.1.9](https://github.com/ash-project/ash/compare/0.1.8...v0.1.9) (2020-06-04)

### Bug Fixes:

- references to error handling code

- fix empty filter checks

## [v0.1.8](https://github.com/ash-project/ash/compare/0.1.7...v0.1.8) (2020-06-02)

This release is a test of our automatic hex.pm package publishing

## [v0.1.7](https://github.com/ash-project/ash/compare/0.1.6...v0.1.7) (2020-06-02)

This release is a test of our automatic hex.pm package publishing

## [v0.1.6](https://github.com/ash-project/ash/compare/0.1.5...v0.1.6) (2020-06-02)

This release is a test of our automatic hex.pm package publishing

## [v0.1.5](https://github.com/ash-project/ash/compare/0.1.4...v0.1.5) (2020-06-02)

This release is a test of our automatic hex.pm package publishing

## [v0.1.4](https://github.com/ash-project/ash/compare/0.1.3...v0.1.4) (2020-06-02)

This release covers the initial linting/dialyzing improvements

## (2020-06-01)

### Changelog Begins
