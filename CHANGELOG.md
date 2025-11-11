<!--
SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs.contributors>

SPDX-License-Identifier: MIT
-->

# Changelog

<!-- changelog -->

## [v3.9.0](https://github.com/ash-project/ash/compare/v3.8.0...v3.9.0) (2025-11-09)




### Features:

* keep tenant in query when using :bypass and :bypass_all (#2429) by [@Malian](https://github.com/Malian) [(#2429)](https://github.com/ash-project/ash/pull/2429)

### Bug Fixes:

* added bulk_action_ref for changeset lookups (#2411) by Daniel Gollings [(#2411)](https://github.com/ash-project/ash/pull/2411)

## [v3.8.0](https://github.com/ash-project/ash/compare/v3.7.6...v3.8.0) (2025-11-05)




### Features:

* tenant_from_attribute dsl option (#2412) by [@barnabasJ](https://github.com/barnabasJ) [(#2412)](https://github.com/ash-project/ash/pull/2412)

### Bug Fixes:

* use correct telemetry span name for notifier invocations by [@zachdaniel](https://github.com/zachdaniel)

* ensure preserve_nil_values? constraint is taking into account in Struct type by [@Malian](https://github.com/Malian) [(#2414)](https://github.com/ash-project/ash/pull/2414)

* Default tenant to changeset.tenant if not set (#2422) by [@shahryarjb](https://github.com/shahryarjb) [(#2422)](https://github.com/ash-project/ash/pull/2422)

* retain calculation boundaries when expanding calculations in by [@zachdaniel](https://github.com/zachdaniel)

* use atomic_upgrade_with for bulk destroys by [@zachdaniel](https://github.com/zachdaniel)

* trailing bypass in policies (#2404) by [@maennchen](https://github.com/maennchen) [(#2404)](https://github.com/ash-project/ash/pull/2404)

### Improvements:

* Add preserve_nil_values? to map and struct types (#2414) by [@Malian](https://github.com/Malian) [(#2414)](https://github.com/ash-project/ash/pull/2414)

* enable configuring match v4 uuids for v7 uuids (#2416) by Kenneth Kostrešević [(#2416)](https://github.com/ash-project/ash/pull/2416)

## [v3.7.6](https://github.com/ash-project/ash/compare/v3.7.5...v3.7.6) (2025-10-19)




### Bug Fixes:

* ensure attribute used in atomic update is casted fully by [@zachdaniel](https://github.com/zachdaniel)

## [v3.7.5](https://github.com/ash-project/ash/compare/v3.7.4...v3.7.5) (2025-10-19)


### Bug Fixes:

* revert changes to bulk action index context/metadata keys


## [v3.7.4](https://github.com/ash-project/ash/compare/v3.7.3...v3.7.4) (2025-10-19)

### Bug Fixes:

* ensure that a filter vs forbidden response is correctly deteermined by [@zachdaniel]

(not a security issue, only affects the response type)


## [v3.7.3](https://github.com/ash-project/ash/compare/v3.7.2...v3.7.3) (2025-10-19)




### Bug Fixes:

* don't re-cast atomic updates when adding validations by [@zachdaniel](https://github.com/zachdaniel)

* ensure calculation contexts include changes from before_transaction hooks by [@zachdaniel](https://github.com/zachdaniel)

### Improvements:

* Simplify forbidden_due_to_strict_policy check (#2400) by [@maennchen](https://github.com/maennchen)

## [v3.7.2](https://github.com/ash-project/ash/compare/v3.7.1...v3.7.2) (2025-10-18)




### Bug Fixes:

* update crux for proper eager check evaluation by [@zachdaniel](https://github.com/zachdaniel)

* clear metadata before for_update and for_destroy actions (#2362) by bipasamaji

* ensure that we detect filter checks failing statically by [@zachdaniel](https://github.com/zachdaniel)

* use *last* changing attribute for atomic validation target by [@zachdaniel](https://github.com/zachdaniel)

## [v3.7.1](https://github.com/ash-project/ash/compare/v3.7.0...v3.7.1) (2025-10-17)




### Bug Fixes:

* normalize bulk index metadata before returning by [@zachdaniel](https://github.com/zachdaniel)

* reuse type constraints when matching relationship records (#2391) by Hannes Wüthrich

## [v3.7.0](https://github.com/ash-project/ash/compare/v3.6.3...v3.7.0) (2025-10-15)




### Features:

* Refactor SAT Solver into `crux` (#2375) by [@maennchen](https://github.com/maennchen)

### Improvements:

* Add combination_acc callback to DataLayer behaviour by [@zachdaniel](https://github.com/zachdaniel)

## [v3.6.3](https://github.com/ash-project/ash/compare/v3.6.2...v3.6.3) (2025-10-15)




### Bug Fixes:

* use `fields` key in atomic errors when exception has fields by [@zachdaniel](https://github.com/zachdaniel)

* use is_nil in ets upsert filter if the value is nil (#2363) by [@barnabasJ](https://github.com/barnabasJ)

* bypass field policy with condition (#2369) by [@maennchen](https://github.com/maennchen)

### Improvements:

* optimize Ash.Resource.Igniter.list_resources and Ash.Domain.Igniter.list_domains for large codebases (#2371) by Elliot Bowes

* optimize list_resources and list_domains for large codebases by Elliot Bowes

* Improve SatSolver.simplify_expression/1 (#2367) by [@maennchen](https://github.com/maennchen)

* Policy Refactoring (#2365) by [@maennchen](https://github.com/maennchen)

## [v3.6.2](https://github.com/ash-project/ash/compare/v3.6.1...v3.6.2) (2025-10-10)




### Bug Fixes:

* properly apply bypass policies that can never pass by [@maennchen](https://github.com/maennchen)
resolves CVE-2025-48043

## [v3.6.1](https://github.com/ash-project/ash/compare/v3.6.0...v3.6.1) (2025-10-10)




### Bug Fixes:

* only add field to error function if not already present by [@zachdaniel](https://github.com/zachdaniel)

## [v3.6.0](https://github.com/ash-project/ash/compare/v3.5.43...v3.6.0) (2025-10-10)




### Features:

* Improve selection of atomic validation attribute, including resource-level option (#2356) by [@stevebrambilla](https://github.com/stevebrambilla)

* data_one_of validation (#2358) by [@barnabasJ](https://github.com/barnabasJ)

* add `Ash.transact/3` (#2341) by [@barnabasJ](https://github.com/barnabasJ)

### Bug Fixes:

* don't stringify fieldset atoms in read action by [@zachdaniel](https://github.com/zachdaniel)

* extended context collision prevention to all bulk operation types and added convenience helpers (#2357) by Daniel Gollings

* improve nested bulk action notification handling (#2353) by Daniel Gollings

* Ash.Query.after_transaction result argument issues. (#2354) by James Harton

* after_transaction on read error bug. by James Harton

* correct after_transaction result type for successful queries. by James Harton

* properly set defaults when using string keys in typed structs by [@zachdaniel](https://github.com/zachdaniel)

* move rollback_on_error logic to DataLayer.transaction by [@barnabasJ](https://github.com/barnabasJ)

* handle return_skipped_upsert? in bulk creates (#2343) by [@barnabasJ](https://github.com/barnabasJ)

* Incorrect return type checking for generic action hooks (#2352) by James Harton

* remove non public arguments in generator action_input (#2350) by Minsub Kim

* handle atomic_refs in bulk upserts (#2342) by [@barnabasJ](https://github.com/barnabasJ)

* fix with_transaction bug in Mnesia data layer (#2340) by Dan Wanek

* properly retrieve `parent` relationship paths out of `exists` by [@zachdaniel](https://github.com/zachdaniel)

### Improvements:

* add can? callback for return_skipped_upsert? for bulk by [@barnabasJ](https://github.com/barnabasJ)

## [v3.5.43](https://github.com/ash-project/ash/compare/v3.5.42...v3.5.43) (2025-09-28)




### Bug Fixes:

* cast value to type when requiring atomic attributes by [@zachdaniel](https://github.com/zachdaniel)

* fix amnesia support for upsets by Dan Wanek

* support top-level `:shared` key in `Ash.Scope.ToOpts` for `Map` (#2261) by James Harton

### Improvements:

* add bulk_create for Ash.DataLayer.Mnesia (#2336) by Dan Wanek

* return errors from calculations with invalid expressions by [@zachdaniel](https://github.com/zachdaniel)

* add source locations to a whole bunch of errors by [@zachdaniel](https://github.com/zachdaniel)

* add location to primary key verifier by [@zachdaniel](https://github.com/zachdaniel)

* add location for multitenancy verifier by [@zachdaniel](https://github.com/zachdaniel)

* add location information for manage relationship transformer by [@zachdaniel](https://github.com/zachdaniel)

* add location info for reserved field names transformer by [@zachdaniel](https://github.com/zachdaniel)

* add location info for primary action transformer by [@zachdaniel](https://github.com/zachdaniel)

* add location info for unique action name transformer by [@zachdaniel](https://github.com/zachdaniel)

* implemented 'has' and 'intersects' functions (#2324) by Abdessabour Moutik

## [v3.5.42](https://github.com/ash-project/ash/compare/v3.5.41...v3.5.42) (2025-09-20)




### Improvements:

* use `delay_task` for `mix ash.setup` by [@zachdaniel](https://github.com/zachdaniel)

## [v3.5.41](https://github.com/ash-project/ash/compare/v3.5.40...v3.5.41) (2025-09-20)




### Bug Fixes:

* use explicit `is_nil` check for `upsert_condition` by [@zachdaniel](https://github.com/zachdaniel)

* actually use the mnesia table configured ð¤¦ââï¸ by [@zachdaniel](https://github.com/zachdaniel)

* add missing capabilities to data layer spec by [@zachdaniel](https://github.com/zachdaniel)

* Typo in scope documentation (#2328) by [@Munksgaard](https://github.com/Munksgaard)

* properly pass action to requires_original_data? in VerifyActionsAtomic (#2327) by Christopher Bonhage

* avoid crashing on new exception (#2322) by Cyprien Poisson

* handle validation init errors without overriding them with validation message (#2320) by marot

### Improvements:

* add `--setup` flag to `ash.install` to run `ash.setup` by [@zachdaniel](https://github.com/zachdaniel)

* Improve performance of mix tasks that load extensions (#2332) by drtheuns

* better error message on invalid relationship paths in exists by [@zachdaniel](https://github.com/zachdaniel)

* add `any` builtin validation by [@zachdaniel](https://github.com/zachdaniel)

* detect types in `get_path` expressions by [@zachdaniel](https://github.com/zachdaniel)

## [v3.5.40](https://github.com/ash-project/ash/compare/v3.5.39...v3.5.40) (2025-09-13)




### Bug Fixes:

* Make Ash.Type.Keyword.dump_to_native return a map. (#2318) by Torkild Gundersen Kjevik

* nif rustler_precompiled: update targets to ubuntu-22.04 and extend release list to prevent stuck builds by Shahryar Tavakkoli

* update targets to ubuntu-22.04 and extend release list to prevent stuck builds (#2311) by Shahryar Tavakkoli

* Runtime upsert_fields option is not passed to data layer in certain cases (#2310) by jlgeering

* if nils_distinct?, consider multitenancy attribute as an upsert key by Zach Daniel

## [v3.5.39](https://github.com/ash-project/ash/compare/v3.5.38...v3.5.39) (2025-09-06)


### Security Vulnerabilities Addressed

* authorize before before_transaction hooks in bulk actions by [@zachdaniel](https://github.com/zachdaniel)

This fixes the CVE: CVE-2025-48042. See the [GHSA-jj4j-x5ww-cwh9 advisory](https://github.com/ash-project/ash/security/advisories/GHSA-jj4j-x5ww-cwh9) for more. 

### Bug Fixes:

* special case ci_strings as strings in type casting by [@zachdaniel](https://github.com/zachdaniel)

* make `Ash.PlugHelpers.set_actor/2` typespec accept term as an actor (#2307) by Maciej Malecki

## [v3.5.38](https://github.com/ash-project/ash/compare/v3.5.37...v3.5.38) (2025-09-04)


### Bug Fixes:

* cache action inputs for all action types by [@zachdaniel](https://github.com/zachdaniel)

* properly pass template opts when showing policy errors by [@zachdaniel](https://github.com/zachdaniel)

* add default values to manual action changesets (#2305) by [@Torkan](https://github.com/Torkan)

* properly escape collection defaults in TypedStruct macro (#2304) by [@bradleygolden](https://github.com/bradleygolden)

### Improvements:

* skip count of records when paginating if filter evaluates to false (#2303) by Rodolfo Torres

## [v3.5.37](https://github.com/ash-project/ash/compare/v3.5.36...v3.5.37) (2025-08-31)




### Bug Fixes:

* ensure that count tasks are always stopped by [@zachdaniel](https://github.com/zachdaniel)

* Allow ^actor() in upsert_condition (#2297) by [@FugiTech](https://github.com/FugiTech)

* properly add aggregate with `add_new_aggregate` by [@zachdaniel](https://github.com/zachdaniel)

* resolve variable swapping in authorizer reducer (#2296) by Jesse Williams

* handle notification without set domain in telemetry spans (#2293) by [@vonagam](https://github.com/vonagam)

* Use `:unsafe_to_atom?` constraint when casting stored atom values by Rutgerdj

* add CompileError on duplicate Ash.Domain use by LambdaCalc

* handle indexed maps when typecasting filter values by [@zachdaniel](https://github.com/zachdaniel)

* deterministic partial evaluation by [@zachdaniel](https://github.com/zachdaniel)

* ensure that unions stored with tags actually have their tags by [@zachdaniel](https://github.com/zachdaniel)

* resolve warning about `nil` atomics by [@zachdaniel](https://github.com/zachdaniel)

* typespec fix for typedstruct info module (#2286) by Matt Beanland

### Improvements:

* properly short-circuit if, || and && by [@zachdaniel](https://github.com/zachdaniel)

## [v3.5.36](https://github.com/ash-project/ash/compare/v3.5.35...v3.5.36) (2025-08-21)




### Bug Fixes:

* revert typed_struct enhancements by [@zachdaniel](https://github.com/zachdaniel)

## [v3.5.35](https://github.com/ash-project/ash/compare/v3.5.34...v3.5.35) (2025-08-21)




### Bug Fixes:

* use `Ash.read!` in usage rules by [@zachdaniel](https://github.com/zachdaniel)

* respect disable_async config during pagination count (#2280) by skanderm

* properly handle overriding `new` for typed struct by [@zachdaniel](https://github.com/zachdaniel)

* don't skip authorization for unrelated aggregates by [@zachdaniel](https://github.com/zachdaniel)

* multiple errors (#2260) by [@TwistingTwists](https://github.com/TwistingTwists)

* avoid String.to_atom by pre-computing mixed argument name MapSet (#2262) by James Harton

* Accept nil input for maps and TypedStructs (#2257) by Fugi

* pass actor: nil to ash.get in get_and_lock_for_update (#2256) by Jesse Williams

* add default impl for rewrite/3 & get_rewrites/4 for Ash.Type. (#2251) by Torkild Gundersen Kjevik

* filter out invalid changeset, before running the bulk before transactions (#2249) by Barnabas Jovanovics

* minor tweaks to usage rules (#2246) by albinkc

* handle updates on union types containing arrays  (#2237) by Rutgerdj

### Improvements:

* introspection and docs for typed structs (#2277) by [@matt-beanland](https://github.com/matt-beanland)

* check change and validate callbacks using behaviour helpers by [@zachdaniel](https://github.com/zachdaniel)

* support "unrelated" aggregates (#2240) by [@zachdaniel](https://github.com/zachdaniel)

* support "unrelated" aggregates by [@zachdaniel](https://github.com/zachdaniel)

* add unrelated exists expressions by [@zachdaniel](https://github.com/zachdaniel)

* TypedStruct required field pattern matching (#2265) by [@chazwatkins](https://github.com/chazwatkins)

* compilation: Move type list module attrs from Ash.Type to Ash.Type.Registry (#2266) by [@chazwatkins](https://github.com/chazwatkins)

* changeset_generator takes scope opt (#2263) by Joseph Lozano

* Support through for many to many relationship gen (#2233) by Kenneth Kostrešević

* Update usage-rules.md to include Polymorphic Relationships (#2211) by Gonzalo Muñoz

* filter expression verifier (#2243) by Kenneth Kostrešević

* Introduce extension introspection functions on Ash.*.Info (#2239) by Jonatan Männchen

## [v3.5.34](https://github.com/ash-project/ash/compare/v3.5.33...v3.5.34) (2025-08-07)




### Bug Fixes:

* avoid String.to_atom by pre-computing mixed argument name MapSet (#2262) by James Harton

* Accept nil input for maps and TypedStructs (#2257) by [@FugiTech](https://github.com/FugiTech)

* pass actor: nil to ash.get in get_and_lock_for_update (#2256) by Jesse Williams

* add default impl for rewrite/3 & get_rewrites/4 for Ash.Type. (#2251) by [@Torkan](https://github.com/Torkan)

* filter out invalid changeset, before running the bulk before transactions (#2249) by [@barnabasJ](https://github.com/barnabasJ)

* minor tweaks to usage rules (#2246) by albinkc

* handle updates on union types containing arrays  (#2237) by Rutgerdj

### Improvements:

* Support through for many to many relationship gen (#2233) by Kenneth Kostrešević

* Update usage-rules.md to include Polymorphic Relationships (#2211) by Gonzalo Muñoz

* filter expression verifier (#2243) by Kenneth Kostrešević

* Introduce extension introspection functions on Ash.*.Info (#2239) by Jonatan Männchen

## [v3.5.33](https://github.com/ash-project/ash/compare/v3.5.32...v3.5.33) (2025-07-29)




### Bug Fixes:

* handle `nil` values properly in atomic changing validations by [@zachdaniel](https://github.com/zachdaniel)

* Application ER Mermaid Diagram (#2231) by [@maennchen](https://github.com/maennchen)

* clear `nil` options in typed struct to constraints process by [@zachdaniel](https://github.com/zachdaniel)

* allow Ash @global_opts authorize? to accept boolean or nil (#2225) by [@chazwatkins](https://github.com/chazwatkins)

* Don't require permissions to read actor on non-atomic `relate_actor` (#2223) by [@chazwatkins](https://github.com/chazwatkins)

* call underlying stream run instead of public interface by [@zachdaniel](https://github.com/zachdaniel)

### Improvements:

* `get_by_id` typo fix in usage rules (#2230) by albinkc

* Add array modifier to `ash.gen.resource` (#2228) by Kenneth Kostrešević

* Add prepend? opt to hooks and Ash.Subject transaction hooks (#2221) by [@chazwatkins](https://github.com/chazwatkins)

* hooks: Add transaction hooks to Ash.Subject by [@chazwatkins](https://github.com/chazwatkins)

* Ash.Subject delegation and improvements by [@chazwatkins](https://github.com/chazwatkins)

* add `log?` option to `can` functions by [@zachdaniel](https://github.com/zachdaniel)

* add transaction hooks for read actions (#2219) by ChristianAlexander

## [v3.5.32](https://github.com/ash-project/ash/compare/v3.5.31...v3.5.32) (2025-07-23)




### Bug Fixes:

* ensure that exists "at_path" is returned as a relationship path by [@zachdaniel](https://github.com/zachdaniel)

### Improvements:

* add before/after/around transaction hooks to generic actions (#2218) by ChristianAlexander

## [v3.5.31](https://github.com/ash-project/ash/compare/v3.5.30...v3.5.31) (2025-07-22)




### Bug Fixes:

* Catch invalid load (#2213) by Kenneth Kostrešević

### Improvements:

* Add Ash.Subject to abstract Changeset, Query, ActionInput common functions (#2212) by [@chazwatkins](https://github.com/chazwatkins)

* support validations, preparations on generic actions by [@zachdaniel](https://github.com/zachdaniel)

* add before/after action hooks to generic actions by [@zachdaniel](https://github.com/zachdaniel)

## [v3.5.30](https://github.com/ash-project/ash/compare/v3.5.29...v3.5.30) (2025-07-18)




### Bug Fixes:

* handle new return value from `kind_of_thing` in gen.resource by [@zachdaniel](https://github.com/zachdaniel)

### Improvements:

* support global validations being applied to reads by [@zachdaniel](https://github.com/zachdaniel)

## [v3.5.29](https://github.com/ash-project/ash/compare/v3.5.28...v3.5.29) (2025-07-18)




### Bug Fixes:

* properly return `:error` when unable to determine type of module by [@zachdaniel](https://github.com/zachdaniel)

## [v3.5.28](https://github.com/ash-project/ash/compare/v3.5.27...v3.5.28) (2025-07-17)




### Bug Fixes:

* accept composite types in typed struct types by [@zachdaniel](https://github.com/zachdaniel)

* keep descriptions from typed struct DSL by [@zachdaniel](https://github.com/zachdaniel)

## [v3.5.27](https://github.com/ash-project/ash/compare/v3.5.26...v3.5.27) (2025-07-17)




### Bug Fixes:

* properly merge contexts from scope and opts by [@zachdaniel](https://github.com/zachdaniel)

* ensure that `modify_query` is honored on aggregates by [@zachdaniel](https://github.com/zachdaniel)

* read action: Tenant from before_action will survive in metadata (#2189) by [@serpent213](https://github.com/serpent213)

* set action on aggregate queries by [@zachdaniel](https://github.com/zachdaniel)

* cascade change action selection (#2193) by [@barnabasJ](https://github.com/barnabasJ)

* try atomic_upgrade_with before falling back to primary action in cascade changes (#2191) by [@barnabasJ](https://github.com/barnabasJ)

* ensure `cast_input` uses `new` callback on typed structs by [@zachdaniel](https://github.com/zachdaniel)

* use correct options in code interfaces in usage-rules.md by [@zachdaniel](https://github.com/zachdaniel)

* support single atoms in `sort_input` by [@zachdaniel](https://github.com/zachdaniel)

### Improvements:

* support where clauses on preparations by [@zachdaniel](https://github.com/zachdaniel)

* support `Ash.Query` for most builtin validations by [@zachdaniel](https://github.com/zachdaniel)

* support validations on read actions by [@zachdaniel](https://github.com/zachdaniel)

* introduce `Ash.TypedStruct` as a simpler struct type by [@zachdaniel](https://github.com/zachdaniel)

## [v3.5.26](https://github.com/ash-project/ash/compare/v3.5.25...v3.5.26) (2025-07-09)




### Bug Fixes:

* don't dump values to native when building changesets by [@zachdaniel](https://github.com/zachdaniel)

* ensure that async limiter is properly cleared by [@zachdaniel](https://github.com/zachdaniel)

* when comparing embedded attributes, ignore metadata keys by [@zachdaniel](https://github.com/zachdaniel)

* clear async limiter on pagination query by [@zachdaniel](https://github.com/zachdaniel)

### Improvements:

* set `show_policy_breakdowns?` to `true` in dev/test by default by [@zachdaniel](https://github.com/zachdaniel)

* make `mix ash.gen.resource` merge existing resources by [@zachdaniel](https://github.com/zachdaniel)

* allow code_interface default_options to accept a function (#2183) by scottwoodall

* show optional code interface inputs in usage rules by [@zachdaniel](https://github.com/zachdaniel)

## [v3.5.25](https://github.com/ash-project/ash/compare/v3.5.24...v3.5.25) (2025-07-02)




### Bug Fixes:

* handle additional cases in `Ash.Filter.flat_map` by [@zachdaniel](https://github.com/zachdaniel)

* handle overlapping type short-codes in app/dependencies by [@zachdaniel](https://github.com/zachdaniel)

* ensure that constraints are set on calculations by [@zachdaniel](https://github.com/zachdaniel)

* Upgrade strip_metadata/1 to recurse on maps and handle additional metadata fields (#2169) by Samuel Wrenn

* ensure that atomic upgrades assume casted inputs by [@zachdaniel](https://github.com/zachdaniel)

* move length constraint checking to the end (#2155) by kernel-io

### Improvements:

* validate reserved constraint key names by [@zachdaniel](https://github.com/zachdaniel)

* Read Action multitenancy :bypass_all (#2154) by xantrac

* add missing types to Ash.load typespec (#2167) by [@barnabasJ](https://github.com/barnabasJ)

* allow pure-binary sigils in expr fragments (#2165) by Frank Dugan III

* add notifier trace/telemetry span type by [@zachdaniel](https://github.com/zachdaniel)

## [v3.5.24](https://github.com/ash-project/ash/compare/v3.5.23...v3.5.24) (2025-06-25)




### Bug Fixes:

* restrict subset of context -> opts in embedded changesets by [@zachdaniel](https://github.com/zachdaniel)

* properly handle nested context for embeddable type by [@zachdaniel](https://github.com/zachdaniel)

* Use the same parent query timeout for the count query, for `read` actions (#2161) by sevenseacat

* ensure join query has shared context passed to it by [@zachdaniel](https://github.com/zachdaniel)

* ensure shared context is set on through queries when loading by [@zachdaniel](https://github.com/zachdaniel)

### Improvements:

* make usage-rules more clear about require Ash.Query by [@zachdaniel](https://github.com/zachdaniel)

* `require Ash.Query` in `Ash.Resource` by [@zachdaniel](https://github.com/zachdaniel)

## [v3.5.23](https://github.com/ash-project/ash/compare/v3.5.22...v3.5.23) (2025-06-19)




### Bug Fixes:

* set proper metadata key for bulk destroy changes by [@zachdaniel](https://github.com/zachdaniel)

* don't override tenant if manually set by [@zachdaniel](https://github.com/zachdaniel)

## [v3.5.22](https://github.com/ash-project/ash/compare/v3.5.21...v3.5.22) (2025-06-18)




### Bug Fixes:

* template opts in conditions in atomic changes by [@zachdaniel](https://github.com/zachdaniel)

* properly detect existing timestamp attributes in igniter tasks by [@zachdaniel](https://github.com/zachdaniel)

### Improvements:

* improve message for --dev migrations message by [@zachdaniel](https://github.com/zachdaniel)

* add `mix ash` command by [@zachdaniel](https://github.com/zachdaniel)

* handle changes to `nil` for unknown attribute values by [@zachdaniel](https://github.com/zachdaniel)

## [v3.5.21](https://github.com/ash-project/ash/compare/v3.5.20...v3.5.21) (2025-06-16)




### Bug Fixes:

* support read actions and arguments in Ash.Generator.action_input/3 (#2137) by [@barnabasJ](https://github.com/barnabasJ)

* combination_of typespec (#2135) by [@barnabasJ](https://github.com/barnabasJ)

### Improvements:

* allow update_query when `expr_error` is nto supported by [@zachdaniel](https://github.com/zachdaniel)

* only require atomicity when update_query *and* expr_error supported by [@zachdaniel](https://github.com/zachdaniel)

* make installer avoid protocol consolidation by [@zachdaniel](https://github.com/zachdaniel)

## [v3.5.20](https://github.com/ash-project/ash/compare/v3.5.19...v3.5.20) (2025-06-13)




### Bug Fixes:

* support private_arguments in code interface and bulk actions (#2133) by [@barnabasJ](https://github.com/barnabasJ)
* combination_of typespec (#2135) by [@barnabasJ](https://github.com/barnabasJ)

## [v3.5.19](https://github.com/ash-project/ash/compare/v3.5.18...v3.5.19) (2025-06-12)




### Bug Fixes:

* bad pattern match in query inspect logic by [@zachdaniel](https://github.com/zachdaniel)

## [v3.5.18](https://github.com/ash-project/ash/compare/v3.5.17...v3.5.18) (2025-06-11)




### Improvements:

* support regexes in match constraint/validation again by [@zachdaniel](https://github.com/zachdaniel)

## [v3.5.17](https://github.com/ash-project/ash/compare/v3.5.16...v3.5.17) (2025-06-10)




### Bug Fixes:

* set_tenant on combination query (#2123) by [@barnabasJ](https://github.com/barnabasJ)

## [v3.5.16](https://github.com/ash-project/ash/compare/v3.5.15...v3.5.16) (2025-06-10)




### Bug Fixes:

* ensure context is properly threaded through bulk update/atomic upgrades by [@zachdaniel](https://github.com/zachdaniel)

* ensure `actor: nil` key is retained on scope to opts by [@zachdaniel](https://github.com/zachdaniel)

* ensure stream_batch_size is properly set to batch_size on stream by [@zachdaniel](https://github.com/zachdaniel)

* set batch size option when streaming by [@zachdaniel](https://github.com/zachdaniel)

* keep union types in order by [@zachdaniel](https://github.com/zachdaniel)

* resolve accidentally backwards incompatible inspect implementation by [@zachdaniel](https://github.com/zachdaniel)

* better error message on mismatch action types in changesets by [@zachdaniel](https://github.com/zachdaniel)

* add clause in Ash.Type.String.match/1 to handle the OTP 28 regex tuples (#2119) by Simon Bergström

* underlying ecto type casting should use coercion, not cast_input by [@zachdaniel](https://github.com/zachdaniel)

* make decimal type less strict by [@zachdaniel](https://github.com/zachdaniel)

* Fix typespecs for Ash.get! and Ash.load! (#2117) by Moxley Stratton

* implement Scope.to_opts for policy authorizer by [@zachdaniel](https://github.com/zachdaniel)

* simplify relationship loading, to prevent losing set query info by [@zachdaniel](https://github.com/zachdaniel)

* typo in Ash.Error.Framework module (#2108) by Samuel Wrenn

* ash.gen.validation generates invalid callback (#2103) by KasparKipp

### Improvements:

* show action when inspecting query by [@zachdaniel](https://github.com/zachdaniel)

* add assert_stripped test helper (#2107) by Samuel Wrenn

* fix misleading value in invalid attribute error by [@zachdaniel](https://github.com/zachdaniel)

* better error message on unexpected argument inputs to code interfaces (#2102) by Abhishek Tripathi

## [v3.5.15](https://github.com/ash-project/ash/compare/v3.5.14...v3.5.15) (2025-06-04)




### Bug Fixes:

* only override bulk options if method is `:id` in code interfaces

* handle missing primary keys more explicitly, and gracefully

* Omit nil values in Ash.Scope.to_opts/2 (#2088)

* Correct error message, example and doc for match constraint. (#2086)

* handle context opt in Ash.calculate/2 (#2083)

### Improvements:

* put __meta__ and relationships at the end when inspecting (#2096)

* usage-rules: Add more guidance around codegen. (#2100)

* update usage rules (#2095)

## [v3.5.14](https://github.com/ash-project/ash/compare/v3.5.13...v3.5.14) (2025-06-01)




### Bug Fixes:

* ensure the proper dependency on igniter

* ensure map module loaded when calling it

* raise if integer primary key is not supplied in ETS (#2079)

## [v3.5.13](https://github.com/ash-project/ash/compare/v3.5.12...v3.5.13) (2025-05-30)


### OTP 28 Compatibility

`Ash.Type.String`, `Ash.Type.CiString` and the built in `match` validation all now warn on the use
of regexes. OTP 28 no longer supports building regexes at compile time. The warning will explain.

## Notable Features

* `:shared` context key is now automatically passed down to child actions. See [the actions guide](/documentation/topics/actions/actions.md#context) for more information.

* add `Ash.Scope`, mirroring the Phoenix pattern, and extend that to all Ash callback contexts. See `Ash.Scope` for more. (#2050)

### Bug Fixes:

* [`Ash.Query`] Allow non-list input to skip_unknown_inputs opt of query (#2074)

* [`Ash.Query`] handle single tuple & string values in sort

* [`Ash.Query`] use proper read action calls when building aggregates

* [`Ash.Generator`] don't generate values for attributes that are `writable?: false` and `generated?: true`

* [`:embedded` resources] ensure we detect all cases where embedded attributes can't be atomically updated

* [`Ash.Type.NewType`] fix issue with expanding constraints for lazy initialized new types

* [`Ash.Changeset`] properly pass tenant in bulk_update to managed_relationships (#2061)

### Improvements:

* [`mix ash.codegen`] add `Ash.Error.Framework.PendingCodegen` error, used for the new `AshPhoenix.Plug.CheckCodegenStatus` plug

* [`Ash.Generator`] support upsert/upsert_identity in changeset generator

* [`usage-rules.md`] Improve LLM rules in `usage-rules.md`. See [usage_rules](https://hexdocs.pm/usage-rules) for more.

* [`Ash.Type.Decimal`] - Add `precision` and `scale` constraints.




## [v3.5.12](https://github.com/ash-project/ash/compare/v3.5.11...v3.5.12) (2025-05-22)




### Features:

* add duration type, functions and operator support (#2036)

### Bug Fixes:

* properly split lazy & non-lazy new type initialization

* Ash.Reactor: Don't import `Ash.Expr` in the bulk update DSL. (#2055)

* don't lift query info to aggregate info on aggregation

* ensure managed relationship context is kept for belongs_to relationships

### Improvements:

* Support manage relationship debug (#2021)

* add `strict_load` key to `Ash.Query.build`

* Add an Ash.OptionsHelpers.calculation_type/0 (#2051)

## [v3.5.11](https://github.com/ash-project/ash/compare/v3.5.10...v3.5.11) (2025-05-20)




### Bug Fixes:

* ensure we fully initialize new types

* Tuple loader and serializer (#2049)

* make sure after_action is called in generate_many (#2047)

* properly pass `select` into combinations

* add_new_code_interface/5 when do block occurs after resource (#2020)

### Improvements:

* add experimental new tool `Ash.data_layer_query`

## [v3.5.10](https://github.com/ash-project/ash/compare/v3.5.9...v3.5.10) (2025-05-15)




### Bug Fixes:

* ensure field policies are logged on success

* various additional fixes for bulk action input ordering

* Fix batch order of bulk_create (#2027)

* make `lazy_init?` a callback so it can be checked on new types

* don't raise error when no policies apply to request

* ensure tenant is set on bulk created records.

* don't try to cast input before cast atomic

* properly prevent embedded attribute updates in atomics

* preserve validation messages in non-atomic-bulk-update validations

* add types for times operator

* properly handle pre-expanded newtype constraints

* shortcircuit queries properly

* only print topic if present (#2013)

### Improvements:

* support :time_usec (#2023)

* support `limit` on has_many relationships (#2016)

## [v3.5.9](https://github.com/ash-project/ash/compare/v3.5.8...v3.5.9) (2025-05-06)




### Bug Fixes:

* shortcircuit queries properly

* ensure that context is set on authorizers in nested field policies

* accept `private_arguments` option in the same way as `defaults` in generators

* handle case where atomic change isn't cleared when converted to static

* handle case where atomic condition isn't applied to generated validations

### Improvements:

* normalize authorizer context in more locations

* aggressively prune ets logs while retaining important info

* combination queries (#2009)

## [v3.5.8](https://github.com/ash-project/ash/compare/v3.5.7...v3.5.8) (2025-04-30)




### Bug Fixes:

* handle `nil` type or invalid types more gracefully in type determination

* properly type expressions based on return values

### Improvements:

* add rem/2 expr (#2004)

* add `Ash.Info` for general application information

## [v3.5.7](https://github.com/ash-project/ash/compare/v3.5.6...v3.5.7) (2025-04-29)




### Bug Fixes:

* Accept field opt in query aggregate (#2001)

* only return the changeset (#2003)

* expand types when typing expressions

* Allow soft-deleting of many-to-many relationships (#1999)

* properly show timeout errors on read transactions

* handle values that generate as `nil` in map generators

* sort lazy loaded records post-linking

* properly attach nested transient calculation dependencies

* be more lax with lazy_init? newtypes

* properly return subtype constraints on constraint call

* handle error cases in atomic changesets better

* CodeInterface: logic error in `get_by` code interfaces. (#1961)

* CodeInterface: logic error in `get_by` code interfaces.

* crash when sorting by aggregates with non-attribute field (#1986)

* handle atomic conditions on `{:atomic, ...}` changes

* ensure data layer is loaded before using `function_exported?/3`

* ensure that bulk callbacks are only called when appropriate

* properly update belongs_to records on `relate_and_update`

### Improvements:

* Skip reading query for filter false (#2002)

* generator for union types

* Add various DSL options for inspecting resources

* hide calculations and aggregates when empty

* hide calculation dep calculations while inspecting

* Raise an `ArgumentError` when generating a changeset using a non-existent action (#1992)

* add `:tuple` builtin type

* set changeset.load on bulk destroy actions

* return the error when atomically cascade destroying

* add a description to map/keyword/struct types

* Remove duplicate impl of default policy functions (#1985)

* eagerly expand aggregates in `Ash.aggregate`

* accept `context` option in generic action code interfaces

## [v3.5.6](https://github.com/ash-project/ash/compare/v3.5.5...v3.5.6) (2025-04-15)




### Bug Fixes:

* properly deduplicate on lazy relationship loading

* use correct exceptions and add missing fields to them (#1960)

* undo incorrect change to trimmed string casting

### Improvements:

* add `:__skip__` value to seed

* Allow update action types in Ash.Generator.generate (#1967)

## [v3.5.5](https://github.com/ash-project/ash/compare/v3.5.4...v3.5.5) (2025-04-14)




### Bug Fixes:

* don't trim strings when `trim?: false` is set

* remove flawed optimization about `is_nil: false`

* properly evaluate ci_string concatenation in Elixir

* don't validate types in gen.resource task.

### Improvements:

* add calculation tools to `Ash.Resource.Igniter`

* support a `load` option on changeset building

## [v3.5.4](https://github.com/ash-project/ash/compare/v3.5.3...v3.5.4) (2025-04-10)




### Bug Fixes:

* ensure after hooks force return_records on bulk update/destroy

* add action to changeset for opts fetching in bulk create

* avoid coercion of datetime to date in start_of_day function (#1958)

* incorrect start_of_day value when timezone specified

## [v3.5.3](https://github.com/ash-project/ash/compare/v3.5.2...v3.5.3) (2025-04-09)




### Bug Fixes:

* avoid defining default actions when actions w/ that name exist

* use the notification logic from bulk update in create (#1951)

* honor stream_options in read code interfaces

* Fix filtering with aggregates refers calculation error (#1954)

* properly provide the changeset to after action hooks

* set calculation context in `Ash.can`

* honor `:*` in skip_unknown_inputs in generic actions

* honor action's `skip_unknown_inputs` in generic actions

* set access_from in cascade_<update/destroy> (#1948)

* maintain order of enum values (#1942)

* carry context around to nested calculations better

* fill templates in more necessary places

* set tenant when building query in managed_relationships

### Improvements:

* optimize args handling in code interfaces

* tasks for generating custom modules (#1940)

## [v3.5.2](https://github.com/ash-project/ash/compare/v3.5.1...v3.5.2) (2025-03-31)




### Bug Fixes:

* match errors on cascade destroy/update

* handle templated opts in bulk update after batch results

* support `refs` and `args` option for `define_calculation`

* don't cast arbitrary maps to structs in struct type

### Improvements:

* add `default_sort` on relationships and queries (#1928)

* new type recursive validation (#1913)

* task for generating custom changes module (#1926)

* better error on `case` in ash expressions (#1927)

* helpful error on incorrectly implemented change modules

* support `custom_input`s in code interfaces

* Ash.Type.Enum - Add optional description and label value â¦ (#1925)

* support `exclude_inputs` for `define_calculation`

* add `exclude_inputs` to code interface definitions

* fix hint for read/generic actions in `NoSuchInput`

## [v3.5.1](https://github.com/ash-project/ash/compare/v3.5.0...v3.5.1) (2025-03-27)




### Bug Fixes:

* when reading a record to simulate an update, merge with data

* more consistent error messaging from present validation

* can't atomically update a query w/ after_action hooks

### Improvements:

* add `Ash.transaction(resources, func, opts)` (#1914)

* set `query_for` context on queries

## [v3.5.0](https://github.com/ash-project/ash/compare/v3.4.73...v3.5.0) (2025-03-26)




### Bug Fixes:

* don't set `accessing_from` on lookups for managed relationships

This *may* be a breaking change for some cases, but ultimately we
felt it was too confusing to leave as it was. The semantics of
`accessing_from` meant that "all actions done as part of `manage_relationship`
were allowed to be performed. This was the intended design, but made it easy
to implement an authorization related bug. Specifcaly, the `on_lookup` behavior
would set that context as well, potentially allowing a user to relate something
to a resource that you did not intend them to be able to see.

### Improvements:

* add `tenant()` filter template expression (#1909)

* `Ash.Domain.Igniter.add_new_code_interface/5`

* add `unsafe_to_atom?` constraint for `Ash.Type.Atom`

* support `expr(exists(relation))` (#1912)

## [v3.4.73](https://github.com/ash-project/ash/compare/v3.4.72...v3.4.73) (2025-03-25)




### Bug Fixes:

* use attribute names, not structs, for retaining ets update attrs

* supply reactor with `nil` values for non-supplied arguments

## [v3.4.72](https://github.com/ash-project/ash/compare/v3.4.71...v3.4.72) (2025-03-25)




### Bug Fixes:

* retain loaded fields on ETS data layer update

* Fixes processing of manual bulk actions, with tests. (#1903)

* Ensure batch_size is correctly set, return values are nil if not requested.

* batch_size cond statement, fix batch -> changeset typo.

* ensure consistent bulk result for `return_records?` and `return_errors?` types.

* add action to base query if not present

* update spark to get missing builders in `spark.formatter`

* handle tenancy for built query in cascade destroy/update

* Improve processing of manual actions during bulk operations (#1883)

### Improvements:

* attach a limit to related queries if `from_many?: true`

* support atomic actions in `can_` code interfaces

* import the `:reactor` dep in `.formatter.exs` on install

* maps,structs,keywords can be atomically updated

* support anonymous functions in `error_handler` option

## [v3.4.71](https://github.com/ash-project/ash/compare/v3.4.70...v3.4.71) (2025-03-21)




### Bug Fixes:

* prefer new loads when loading relationships

* put notifications in process context when inside an action

* consider query tenant when validating aggregate multitenancy

* update Validations.ActionIs to accept atom or list(atom) (#1893)

### Improvements:

* set `bulk_actions_default_to_errors?` to `true` in installer

## [v3.4.70](https://github.com/ash-project/ash/compare/v3.4.69...v3.4.70) (2025-03-20)




### Bug Fixes:

* compose get_by and action filters properly

* `fields` could be nil on exceptions

* apply runtime supplied loads over top of action loads

* return records from bulk soft destroy if requested (#1884)

* ensure error classes are used in code interfaces

### Improvements:

* validate multitenancy earlier in bulk actions

* initialize all types properly at compile time

## [v3.4.69](https://github.com/ash-project/ash/compare/v3.4.68...v3.4.69) (2025-03-18)




### Bug Fixes:

* apply strict load in `Ash.get` properly (#1881)

* honor tenant in `Ash.Seed.update!`

* pattern match error in bulk result

* match on `{:not_atomic` pattern in update actions

* fix typo: `atomcis` -> `atomics`

* properly evaluate `Exists{}` expressions in runtime filter

* Fix upsert identity type error (#1872)

* Fix bulk_create for manual create actions with bulk_create/3 (#1869)

* ensure loading maps & structs properly load as keyword

* make the --example installer flag idempotent

* don't add tenant attribute for `all_tenant?` identities upsert keys

* handle case clause error in `Ash.Filter`

* validate return type of `Ash.Resource.Calculation.init/1`

* make `lazy_init` work on `NewTypes`

* properly parse maps with tuple values in filter parser

* dump values to native storage type when doing atomic upgrades

* fix required error deduplication logic

* undo breaking change, use primary read action loads in `Ash.load!`

* don't derive fields for resources in map types

* make sequence unshrinkable in generators

* properly handle bulk soft destroy (#1854)

### Improvements:

* support a resource & attrs tuple in `seed_generator`

* allow is_equal and is_not_equal for compare validation (#1853)

* return errors and stop on errors by default in bulk actions

* add config to do read after_action hooks in order

* add compile flag requiring atomic for default actions

* types support atomic update by default for non-expr values

* add `actions.read.pagination.stable_sort` customization

* support inferring struct types from a resource

* show value in match validation errors

* disambiguation message on NoSuchFunction

## [v3.4.68](https://github.com/ash-project/ash/compare/v3.4.67...v3.4.68) (2025-03-11)




### Bug Fixes:

* init nested types in map/keyword/struct types

* properly handle change with `where` validations in bulk (#1843)

* properly construct `parent_stack` for loaded relationships

* update context tenant from changeset for each change (#1837)

### Improvements:

* allow manual reads return `full_count` for pagination

* validate action types in `Ash` functions

## [v3.4.67](https://github.com/ash-project/ash/compare/v3.4.66...v3.4.67) (2025-03-04)




### Bug Fixes:

* Pass tenant option when seeding resource given by the generator (#1834)

## [v3.4.66](https://github.com/ash-project/ash/compare/v3.4.65...v3.4.66) (2025-03-03)




### Bug Fixes:

* handle unparseable relationship sorts & single atom sorts

* don't allow modifying changeset with atomic conditions

* don't prevent changing values to `nil` when original data is not available

* propagate invalid reference error when adding calc context to sort (#1827)

* ensure that we don't try to compare not loaded or forbidden values

* use `filter` not `filters` in stale record error

### Improvements:

* add `touching?` option to changing validation

* don't show required errors for fields with other errors

* validate aggregate multitenancy

* ignore action-defined loads when using `Ash.load`

## [v3.4.65](https://github.com/ash-project/ash/compare/v3.4.64...v3.4.65) (2025-02-25)




### Bug Fixes:

* properly enumerate `:_pkey` in notifier

* Always rollback input.resource when running generic actions. (#1817)

* fix case where batch before/after action callbacks could be skipped

* return NotFound error in proper cases on bulk interfaces

* don't eagerly return records on bulk update/destroys

* ensure actor templates are hydrated for aggregates

* properly use operator overloads for evaluating operators at runtime

* Missing case clause for bulk update/destroy with `get?: true` in interface (#1806)

* always run update filter on skipped updates

* don't change update defaults unless something changes

### Improvements:

* better parent resource tracking in expressions

* Generic actions to raise if they don't have return type but have an return value (#1805)

## [v3.4.64](https://github.com/ash-project/ash/compare/v3.4.63...v3.4.64) (2025-02-17)




### Bug Fixes:

* use undo action in generic action undo

* handle generic actions with no return

* ensure atomic `set_attribute` behaves the same as non-atomic

* Missing actor on aggregate resource call (#1796)

* Missing actor on aggregate call (#1793)

### Improvements:

* support receiving the inputs when undoing generic actions

* simplify & unify sort/sort_input logic

* support related sorts everywhere (not just sort_input)

* add field names to identities (#1786)

## [v3.4.63](https://github.com/ash-project/ash/compare/v3.4.62...v3.4.63) (2025-02-11)




### Bug Fixes:

* set `read_after_writes` to true if generated

* type cast errors w/ floats & vectors

* handle case clause error in filters

* don't return invalid type from vector type

* don't double process string interpolation expressions

* Include warning for arguments only when 'things' are arguments (#1785)

* empty bulk create inputs must still return a stream

* don't use `authorize_with: :error` on data layers that can't do it

* pass tenant to load in cascade destroy (#1775)

* add tenant to load in cascade update (#1773)

* raise errors on partial_success results in bulk actions

* add `reuse_values?` opt to `Ash.can` and disable it automatically

* compile `Ash.PlugHelpers` even without `Plug` available

* expand `opts` when using `calculate/3`

* handle base resources in `ash.extend`

* type system warning on apps w/o solvers

* Fix no read action exception for through relationship (#1750)

* always recompile domain on resource changes

* fix handling of generic action returns with transaction enabled (#1758)

* type struct handle instance of return error tuple (#1756)

* Compilation failure when using the `ash_step` Reactor DSL. (#1753)

* pass `authorize?` option to `bulk_create` in `Ash.Generator.generate_many/2`

### Improvements:

* Add string_position expression (#1782)

* add `dimensions` to vector type

* add filter & transform options for pubsub notifier

* verify pub_sub actions at compile time

* more clean boolean filter optimization for `or ==`

* add `reuse_values?` option for calculate!

* prefer `calculate/3` when reusing values

* add `c:Ash.Type.coerce/2` callback

## [v3.4.62](https://github.com/ash-project/ash/compare/v3.4.61...v3.4.62) (2025-01-31)




### Bug Fixes:

* [`Ash.Changeset`] always start transactions when managing relationships

* [`Ash.Changeset`] handle parent in rel in managed belongs to (#1746)

## [v3.4.61](https://github.com/ash-project/ash/compare/v3.4.60...v3.4.61) (2025-01-31)




### Bug Fixes:

* [`Ash.Generator`] don't prevent setting manage_relationship inputs in generators

* [`Ash.Reactor`] Fix referring to outer steps and inputs in `transaction` steps. (#1741)

* [`Ash.Expr`] always return utc timestamp as result of `start_of_day`

* [`Ash.Resource.Change.CascadeDestroy`] support `after_action?` option on cascade destroys & better error when it should be used (#1734)

### Improvements:

* [`Ash.Resource`] warning on args, preparations or filters on primary reads

* [`Ash.Reactor`] Support guards in `Ash.Reactor` steps. (#1739)

* [`mix ash.extend`] use `ash.extend` and use it instead of `ash.patch.extend`

## [v3.4.60](https://github.com/ash-project/ash/compare/v3.4.59...v3.4.60) (2025-01-27)




### Bug Fixes:

* [`Ash.Expr`] traverse custom expressions when listing refs

## [v3.4.59](https://github.com/ash-project/ash/compare/v3.4.58...v3.4.59) (2025-01-27)




### Bug Fixes:

* [`Ash.Query`] better placed validations of aggregate support for data layers

## [v3.4.58](https://github.com/ash-project/ash/compare/v3.4.57...v3.4.58) (2025-01-26)




### Bug Fixes:

* [`Ash.Query.Aggregate`] properly check query aggregate support

### Improvements:

* [`Ash`] support `authorize_with` option in `Ash.get` (#1732)

## [v3.4.57](https://github.com/ash-project/ash/compare/v3.4.56...v3.4.57) (2025-01-23)




### Bug Fixes:


* [`Ash.Resource.Validation.Compare`] ensure compare validation doesn't put functions in exceptions


## [3.4.56](https://github.com/ash-project/ash/compare/v3.4.55...3.4.56) (2025-01-21)




### Bug Fixes:


* [`Ash`] don't use `JSON` due to library compatibility issues

* [`Ash.Changeset`] matching in managed_relationships handle_update (#1719)

* [`Ash.Query.Calculation`] properly load doubly nested calculation's explicit dependencies

* [`Ash.Query.Calculation`] handle related non-expr calculations referenced from expr calcs

* [`Ash.Query.Calculation`] simplify and fix path generation for nested relationship path deps

* [`Ash`] don't require multitenancy attribute in `get` (#1716)

### Improvements:

* [`Ash.Changeset`] make atomics work even if expr err is not supported (#1718)

* [`Ash.Query`] support error shorthand for `Ash.Query.add_error/2-3`

* [`Ash.Generator`] add `uses` option for `changeset_generator`

* [`Ash.Generator`] add `uses` option for `seed_generator`

* [`Ash.Changeset`] Use clearer error message for match validation atomic errors (#1721)

* [`Ash.Type`] Add autogenerate_enabled? to Ash.Type for Ecto compatibility (#1715)

* [`Ash.Policy.Authorizer`] warn when domain policies would be ignored by resources

* [`Ash.Domain`] allow policy authorizer to be in authorizers key in domains

## [v3.4.55](https://github.com/ash-project/ash/compare/v3.4.54...v3.4.55) (2025-01-13)




### Bug Fixes:

* [code interfaces] ensure can_* code interfaces pass arguments to actions

* [`Ash`] case clause error in `Ash.can?`

* [`Ash`] reset `ash_started_transaction?` on bulk create

* [`Ash.Generator`] handle embedded attributes in attribute generator

* [`Ash.Generator`] Fix typo in skipped import name (#1704)

* [`Ash.Generator`] set max_concurrency to 0 for generate_many

* [`Ash.Generator`] ensure that `once` and `sequence` behave predictably across tests

### Improvements:

* [`Ash.Changeset`] destroy missing records first in `manage_relationship`

* [`Ash.Expr`] add start_of_day function

* [`Ash.Type.DateTime`] add `cast_dates_as` constraint to `Ash.Type.DateTime`

## [v3.4.54](https://github.com/ash-project/ash/compare/v3.4.53...v3.4.54) (2025-01-09)




### Bug Fixes:

* [`Ash.Generator`] Fix issues in `Ash.Generator.generate_many/2` (#1703)

* [`Ash.Generator`] Don't error if no `after_action` is provided to `generate_many`

* [`Ash.Generator`] Reuse the changeset actor when calling `bulk_create`

### Improvements:

* [`Ash.Generator`] run notifications for generators

* [`Ash.Changeset`] `order_is_key` option for sorted relationships

## [v3.4.53](https://github.com/ash-project/ash/compare/v3.4.52...v3.4.53) (2025-01-08)


### Bug Fixes:

* [`Ash.Generator`] properly delegate and handle conflicts in `Ash.Generator`

* [`Ash.Generator`] Replace calls to `create` and `create_many` with `generate` and `generate_many` (#1701)

* [calculations] use nested calculation dependencies from expr if not in expression

* [`Ash.Changeset`] pattern match error on expression parse failure

* [`Ash.Test.Resource.Validation.StringLengthTest`] handle `string_length` on arguments when atomic

## [v3.4.52](https://github.com/ash-project/ash/compare/v3.4.51...v3.4.52) (2025-01-06)


### Bug Fixes:

* [`Ash.Type.Map`]  handle keyword errors from map field type casting

* [`mix ash.gen.resource`] ensure extensions & subjects args are unique

### Improvements:

* [`ash.gen.resource`] validate that names given to `ash.gen.resource`

* [`Ash.Generator`] add `Ash.Generator.changeset_generator/3`

* [`Ash.Generator`] add `Ash.Generator.seed_generator/2`

* [`Ash.Generator`] only use known keys in generators in `Ash.Generator`

* [`Ash`] support `after_action` option to `Ash.bulk_create`

* [`mix ash.install`] set `yes_to_deps` when fetching dependencies

* [`Ash.Query`] better error message on non-resource in `Ash.Query.new/2`

* [`Ash.bulk_destroy`] handle limited bulk destroys from streams

* [Code interfaces] bulk actions use `full_read` from code interfaces given ids

* [Code Interfaces] set `limit` in code interface to update or destroy one thing

## [v3.4.51](https://github.com/ash-project/ash/compare/v3.4.50...v3.4.51) (2025-01-03)


### Bug Fixes:

* [`Ash.Resource`] handle ambiguous case of empty params in code interfaces (#1694)

* [`Ash.Changeset`] discard manage_relationships added inside changes on atomic upgrade

## [v3.4.50](https://github.com/ash-project/ash/compare/v3.4.49...v3.4.50) (2025-01-01)


### Bug Fixes:

* [`Ash.DataLayer.Ets`, `Ash.DataLayer.Mnesia`] properly handle aggregate defaults in ets/mnesia (#1684)

* [`Ash.Resource.Validation.Changing`] use context message instead of default if provided in changing validation (#1677)

* [`Ash.Changeset`] ensure that `changed?` context is set to true for atomics

* [`Ash`] properly match on `return_query?` option, avoid raised pattern match error

* [`Ash.Policy.Authorizer`] ensure that old config applies all aggregate policies

### Improvements:

* [`Ash.Generator`] add `Ash.Generator.once/2`

* [`Ash.Type.Map`, `Ash.Type.Keyword`, `Ash.Type.Struct`] define `generate/1` callback for maps, structs, keywords

* [`Ash`] add `data_layer?` option to `Ash.calculate/3`

* [`Ash.Resource`] Add default code interface options (#1681)

* [`Ash.Resource`] add `allow_forbidden_field?` option to relationships

* [`Ash.Resource`] add `authorize_read_with` option to relationships

* [`Ash`] support `default` option in `Ash.first` (#1683)

* [`Ash.Notifier.PubSub`] allow exclusion of certain actions from publish_all (#1680)

* [`mix igniter.install ash`] no prompt about SAT solver unless user is on windows

* [`Ash.Domain`] add otp_app option to use Ash.Domain

* - [`Ash`] add support for `strict?` in read options (#1669)

## [v3.4.49](https://github.com/ash-project/ash/compare/v3.4.48...v3.4.49) (2024-12-22)


### Improvements:

- [read actions] - add support for `strict?` in `Ash.read` options. (#1669)

### Bug Fixes:

* [`Ash.Policy.Authorizer`] ensure that old config applies all aggregate policies

If you've upgraded to the following configuration this does not affect you:

```elixir
config :ash, :policies, no_filter_static_forbidden_reads?: false
```

You should upgrade regardless, and adopt that new configuration.

## [v3.4.48](https://github.com/ash-project/ash/compare/v3.4.47...v3.4.48) (2024-12-20)

### Bug Fixes:

- [calculations] properly update sort calculation expressions

- [`Ash.Type.Module`] handle nil values in `Ash.Type.Module`

- [`Ash.Resource`] ensure that `select_by_default?` is honored on loads

- [`Ash.Type.Union`] Verify union types constraint on init

- [loading data] ensure tenant is set on reselection query

### Improvements:

- [Igniter] handle igniter not being compiled, and make it optional

- [`Ash.Generator`] add `Ash.Generator.next_in_sequence/3`

- [performance] don't reselect unnecessary attributes

- [pagination] add `show_keysets_for_all_actions?` configuration

  Set `config :ash, show_keysets_for_all_actions?, false` for significant performance
  improvements when reading resources that support keyset pagination. This causes
  keysets to only be shown for actions that are actively being paginated with
  keyset pagination.

## [v3.4.47](https://github.com/ash-project/ash/compare/v3.4.46...v3.4.47) (2024-12-17)

### Bug Fixes:

- [`Ash.Query`] handle indexed maps and string keys in calculation arguments

- [`Ash.Changeset`] throw validation error when trying to set public arguments in private_arguments (#1663)

- [`Ash.Policy.Authorizer`] include `changeset` in preflight authorization context

- [embedded resources] include presence of authorizers in embedded resource optimization

- [`Ash.DataLayer`] don't check data layer compatibility for manual actions

### Improvements:

- [`Ash.Reactor`]: Always add the notification middleware any time the extension is added. (#1657)

## [v3.4.46](https://github.com/ash-project/ash/compare/v3.4.45...v3.4.46) (2024-12-12)

### Bug Fixes:

- [`Ash.Tracer`] use proper telemetry name for actions

- [`Ash.Sort`] use atoms for paths in related sorts

## [v3.4.45](https://github.com/ash-project/ash/compare/v3.4.44...v3.4.45) (2024-12-10)

### Bug Fixes:

- [`Ash`] don't ignore tenant when calling aggregate functions

### Improvements:

- [`Ash.Policy.Authorizer`] don't log field policies unless logging successful policy breakdowns

## [v3.4.44](https://github.com/ash-project/ash/compare/v3.4.43...v3.4.44) (2024-12-06)

### Bug Fixes:

- [`Ash.Changeset`] use `Ash.read` when eager validating relationships

- [`Ash.Expr`] allow strings in `get_path/2`

- [`Ash.Sort`] don't expand calculations until after authorization is complete

- [`Ash.Resource.Change.GetAndLock`] don't automatically skip `get_and_lock` changes

- [`Ash.Filter`] handle indexed maps in filter map syntax

- [`Ash.Filter`] handle case where `%{or` is composing a single map

- [`Ash.Policy.Authorizer`] ensure that `subject` is properly set when running field policies

- [`Ash.Type`] fix logic errors in matches_type?/list logic

- [pagination] add tenant to Aggregate opts when building count query (#1630)

- [notifications] some notifications not being sent for bulk create actions

- [validations] negate atomic validation expressions when used as `where` conditions (#1624)

- [`Ash.Policy.Authorizer`] don't double apply action-filters when attaching policy filters (optimization)

- [`Ash.read`] Fix not working skip_unknown_inputs opt of read action (#1596)

- [`Ash.read`] Fix warning when actions.read.argument constraint is violated (#1607)

- [aggregates] use last relationship's read action properly in aggregate queries

- [`Ash.Changeset`] finish conversion from `append?` to `prepend?` option (#1601)

### Improvements:

- [`Ash.Type.NewType`] add `lazy_init?` option. Allows for recursive embedded types.

- [`mix ash.gen.resource`] More Descriptive Error Messages for ash.gen.resource (#1645)

- [`Ash.Expr`] better type signatures for division

- [`Ash.Expr`] converge on known types better

- [`Ash.Changeset`] add `Ash.Changeset.force_delete_argument/2`

- [`Ash.Policy.Check.Builtins`] add `actor_absent` builtin check

- [`Ash.Changeset`] Warn when `manage_relationship` is called without opts (#1408)

- [`Ash.Resource.Validation.Builtins`] use Comp with attribute_equals and attribute_does_not_equal (#1623)

## [v3.4.43](https://github.com/ash-project/ash/compare/v3.4.42...v3.4.43) (2024-11-20)

### Bug Fixes:

- [generic actions] store notifications from simple results (#1591)

## [v3.4.42](https://github.com/ash-project/ash/compare/v3.4.41...v3.4.42) (2024-11-07)

### Bug Fixes:

- [`mix ash.generate_livebook`] Allow multi-line descriptions to be rendered in Livebook without error (#1590)

- [`Ash.Policy.Check.ChangingAttributes`] properly handle `changing_attributes` check with `from` in create (#1584)

### Improvements:

- [`Ash.Type.Union`] support map w/ keys `_union_type` and `_union_value` for union inputs

## [v3.4.41](https://github.com/ash-project/ash/compare/v3.4.40...v3.4.41) (2024-11-05)

### Bug Fixes:

- [`Ash.Type.Struct`] don't double wrap casted struct instances in `{:ok, {:ok, ...}}`

- [`Ash.Type.Struct`] support mixed key types in input maps for structs

## [v3.4.40](https://github.com/ash-project/ash/compare/v3.4.39...v3.4.40) (2024-11-04)

### Bug Fixes:

- [`Ash.Actions.Read`] don't call `.name` on a potentially `nil` action

- [`Ash.Expr`] properly detect `Ash.CustomExpression` as an expr

- [`Ash.Expr`] accept fragments pointing at non-aliases

- [`Ash.Expr`] smarter type detection, preferring more concrete types

## [v3.4.39](https://github.com/ash-project/ash/compare/v3.4.38...v3.4.39) (2024-11-01)

### Bug Fixes:

- [`Ash.Changeset`] emit warnings about already validated actions in before_action hooks too

- [`Ash.Changeset`] add missing case clause for `Changeset.atomic_update` type cast (#1569)

- [`Ash.Type.Map`] handle case of invalid map/keyword key when type casting

- [`Ash.Type.Keyword`] handle case of invalid map/keyword key when type casting

### Improvements:

- [error messages] more better error bread crumbs

## [v3.4.38](https://github.com/ash-project/ash/compare/v3.4.37...v3.4.38) (2024-10-31)

### Bug Fixes:

- [`Ash.Changeset`] detect transaction hooks added by around_transaction and before_transaction & manual actions

- [`Ash.Changeset`] don't special case `nil` change on `force_change_attribute`

### Improvements:

- [`Ash.Domain`] default backwards compatible interface to false for domains

- [`Ash.Changeset`, create actions] more and better bread crumbs for changesets & create actions

## [v3.4.37](https://github.com/ash-project/ash/compare/v3.4.36...v3.4.37) (2024-10-30)

### Bug Fixes:

- [`Ash.Type.Union`] handle nil union changing to nil

- [multitenancy] enforce multitenancy on bulk creation

- [`Ash.Changeset`] force_change_attribute no longer cares what the old value is

### Improvements:

- [`Ash.Changeset`] allow specifying `return_skipped_upsert?` as an option to changeset

- [`Ash.DataLayer`] add `prefer_transaction_for_atomic_updates?` data layer callback

- [`Ash.DataLayer`] support `prefer_transaction?` on DataLayer

- [loading data] allow data loading when no primary read action exists

## [v3.4.36](https://github.com/ash-project/ash/compare/v3.4.35...v3.4.36) (2024-10-24)

### Bug Fixes:

- [`Ash.stream!`] ensure opts are passed through to constructed query in stream

### Improvements:

- [`Ash.Policy.Check.Builtins`] add `just_created_with_action/1` check

## [v3.4.35](https://github.com/ash-project/ash/compare/v3.4.34...v3.4.35) (2024-10-22)

### Bug Fixes:

- [code interfaces] allow optional code interface args for fields with defaults

- [atomic updates] write all attributes to atomics list before dispatching to data layer on update query

- [`mix ash.gen.resource`] remove multichar aliases from `mix ash.gen.resource`

- [`Ash.Type.Decimal`] check nil before calling Decimal.eq? in Type.Decimal.equal? (#1538)

## [v3.4.34](https://github.com/ash-project/ash/compare/v3.4.33...v3.4.34) (2024-10-21)

### Bug Fixes:

- [`mix ash.gen.resource`] properly accept options for `ash.gen.resource` in installer

## [v3.4.33](https://github.com/ash-project/ash/compare/v3.4.32...v3.4.33) (2024-10-18)

### Bug Fixes:

- [bulk updates] apply attribute multitenancy on bulk update queries

- [`Ash.Type.Decimal`] use Decimal.eq? in Ash.Type.Decimal (#1532)

- [`Ash.Reactor`]: Don't validate `inputs` keys when being transformed. (#1527)

- [atomic updates] set argument defaults in fully atomic changesets

- [`Ash.Changeset`] ensure that default values are included in attribute changes

- [manage_relationship] properly unrelate belongs_to relationships

- [manage_relationship] ensure unrelated records are removed from the current records list

### Improvements:

- [`Ash.Resource`] Conditionally enable transactions on default actions. (#1525)

- [`Ash.Seed`] ash seed upsert! function (#1522)

- [code interfaces] Add compile-time checks for `code_interface` arguments in Resource and Domain (#1523)

## [v3.4.32](https://github.com/ash-project/ash/compare/v3.4.31...v3.4.32) (2024-10-14)

### Improvements:

- [`mix ash.gen.resource`] use new `:csv` option type from igniter

## [v3.4.31](https://github.com/ash-project/ash/compare/v3.4.30...v3.4.31) (2024-10-14)

### Bug Fixes:

- [all actions] allow strings in generic action skip_unknown_inputs

## [v3.4.30](https://github.com/ash-project/ash/compare/v3.4.29...v3.4.30) (2024-10-14)

### Bug Fixes:

- [all actions] add `tracer` option to generic action opts

## [v3.4.29](https://github.com/ash-project/ash/compare/v3.4.28...v3.4.29) (2024-10-13)

### Bug Fixes:

- [`Ash.Changeset`] clear change from atomics as well

- [read actions] properly invoke notify callback in read actions

### Improvements:

- [`Ash.Changeset`] better ergonomics for atomic updates

- [changeset, action inputs, queries] add `private_arguments` option

- [`Ash.ActionInput`] validate `Ash.ActionInput.for_action` opts

- [`Ash.Type.NewType`] allow additional callbacks in `Ash.Type.NewType`

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

- [Ash.Type.Integer] use correct constraint when validating min int (#1298)

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

- [identities] support `where` option on `identities`

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
- [Ash] we now call functions on this, instead of the domain. i.e `Ash.create` and `Ash.read`. The generated functions are now marked as deprecated
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
