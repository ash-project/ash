# Ash Org Open PRs & Issues
Generated: 2026-02-22 18:47:11
Total: 33 repos with open items, 93 PRs, 344 issues

---

## Waiting (on contributors)

- [ ] ash_phoenix#310: Add Multitenancy Support to gen.live & gen.html (PR #459 open) <!-- https://github.com/ash-project/ash_phoenix/issues/310 -->
- [ ] ash_phoenix PR #459: Add multitenancy to gen.html and gen.live <!-- https://github.com/ash-project/ash_phoenix/pull/459 -->
- [ ] spark PR #253: Builder API for Sections and Entities <!-- https://github.com/ash-project/spark/pull/253 -->

---

## Roadmap

Things to tackle later:

- [ ] ash_sql#132: refactor: use `{path, type, integer}` for start_bindings to avoid shadowing <!-- https://github.com/ash-project/ash_sql/issues/132 -->
- [ ] ash_mysql: review 6 stale dep-bump PRs and issue #25 (problem with indices on `:string` fields) <!-- https://github.com/ash-project/ash_mysql -->

---

## ash_json_api (2 PRs, 18 issues)

### PRs
- [ ] PR #403: docs: add modify-conn guide to generated documentation <!-- https://github.com/ash-project/ash_json_api/pull/403 -->
- [ ] PR #378: Enhance OpenAPI documentation with prefix and security info <!-- https://github.com/ash-project/ash_json_api/pull/378 -->

### Issues
- [ ] Issue #382: feat: Non-atomic bulk operation support <!-- https://github.com/ash-project/ash_json_api/issues/382 -->
- [ ] Issue #381: feat: Add support for ash calculation `field?: false` option <!-- https://github.com/ash-project/ash_json_api/issues/381 -->
- [ ] Issue #366: User defined exception of `class: :forbidden` is swallowed by default implementation <!-- https://github.com/ash-project/ash_json_api/issues/366 -->
- [ ] Issue #349: GET routes for Generic Actions do not assume the arguments as query params <!-- https://github.com/ash-project/ash_json_api/issues/349 -->
- [ ] Issue #310: Allow partial filters on embedded resources in OpenAPI schema generation <!-- https://github.com/ash-project/ash_json_api/issues/310 -->
- [ ] Issue #309: Limit OpenAPI filter operators based on attribute/argument type <!-- https://github.com/ash-project/ash_json_api/issues/309 -->
- [ ] Issue #286: PATCH action requires all required attributes to be provided <!-- https://github.com/ash-project/ash_json_api/issues/286 -->
- [ ] Issue #277: Allow better customization of error status <!-- https://github.com/ash-project/ash_json_api/issues/277 -->
- [ ] Issue #276: Client provided `id` handling changes <!-- https://github.com/ash-project/ash_json_api/issues/276 -->
- [ ] Issue #272: Include `tag_value` as discriminator attribute when writing Ash.Type.Union to OpenAPI <!-- https://github.com/ash-project/ash_json_api/issues/272 -->
- [ ] Issue #251: Allow `destroy` routes to point at `update` actions. <!-- https://github.com/ash-project/ash_json_api/issues/251 -->
- [ ] Issue #236: Add action metadata to the meta field <!-- https://github.com/ash-project/ash_json_api/issues/236 -->
- [ ] Issue #230: Implement pagination feature for related routes <!-- https://github.com/ash-project/ash_json_api/issues/230 -->
- [ ] Issue #202: Can't provide descriptions for routes at the route level  <!-- https://github.com/ash-project/ash_json_api/issues/202 -->
- [ ] Issue #164: JSON:API spec compliance when creating resources - The resource object MUST contain at least a type member. <!-- https://github.com/ash-project/ash_json_api/issues/164 -->
- [ ] Issue #28: Support fields on join tables <!-- https://github.com/ash-project/ash_json_api/issues/28 -->
- [ ] Issue #24: Consider building a regex or all valid versions of includes to validate in the json schema <!-- https://github.com/ash-project/ash_json_api/issues/24 -->
- [ ] Issue #17: Support member name transformers <!-- https://github.com/ash-project/ash_json_api/issues/17 -->

---

## ash_graphql (3 PRs, 21 issues)

### PRs
- [ ] PR #400: Improve error message for generic actions used in <!-- https://github.com/ash-project/ash_graphql/pull/400 -->
- [ ] PR #343: Add failing test for a calculation with a :struct return type <!-- https://github.com/ash-project/ash_graphql/pull/343 -->
- [ ] PR #294: feat: Add validation queries (WIP) <!-- https://github.com/ash-project/ash_graphql/pull/294 -->

### Issues
- [ ] Issue #405: Relay IDs on graphql filters and sorting <!-- https://github.com/ash-project/ash_graphql/issues/405 -->
- [ ] Issue #378: if a resource's `type` is `subscription`, the generated schema conflicts with the subscription schema <!-- https://github.com/ash-project/ash_graphql/issues/378 -->
- [ ] Issue #361: feat: Add support for ash calculation `field?: false` option <!-- https://github.com/ash-project/ash_graphql/issues/361 -->
- [ ] Issue #357: Global graphql type definition <!-- https://github.com/ash-project/ash_graphql/issues/357 -->
- [ ] Issue #354: Subscription DSL filter causes filtered events to become errors <!-- https://github.com/ash-project/ash_graphql/issues/354 -->
- [ ] Issue #350: [Relay] Issue with subscriptions on read_actions which have auth filters and pagination <!-- https://github.com/ash-project/ash_graphql/issues/350 -->
- [ ] Issue #339: Relay support discontinued? <!-- https://github.com/ash-project/ash_graphql/issues/339 -->
- [ ] Issue #315: Proposal: Better error message when "holding it wrong" for mapping up actions <!-- https://github.com/ash-project/ash_graphql/issues/315 -->
- [ ] Issue #292: Add support for descriptions for `Ash.Type.Enum` and `Ash.Type.NewType` types <!-- https://github.com/ash-project/ash_graphql/issues/292 -->
- [ ] Issue #277: Introduce a bulk option (or similarly named) option to updates and potentially destroys <!-- https://github.com/ash-project/ash_graphql/issues/277 -->
- [ ] Issue #269: 2.0: Align placement of erros between generic and other actions <!-- https://github.com/ash-project/ash_graphql/issues/269 -->
- [ ] Issue #248: Input type wrapper for generic action arguments is nullable <!-- https://github.com/ash-project/ash_graphql/issues/248 -->
- [ ] Issue #223: Cryptic warning when generating an input with not fields <!-- https://github.com/ash-project/ash_graphql/issues/223 -->
- [ ] Issue #221: errors do not conform to the GraphQL specification  <!-- https://github.com/ash-project/ash_graphql/issues/221 -->
- [ ] Issue #115: Better error for typos in Aggregates when using AshGraphQL <!-- https://github.com/ash-project/ash_graphql/issues/115 -->
- [ ] Issue #102: Validation errors should include a path <!-- https://github.com/ash-project/ash_graphql/issues/102 -->
- [ ] Issue #99: Add support for Relay refetching (Relay-compliant IDs and the `Node` query) <!-- https://github.com/ash-project/ash_graphql/issues/99 -->
- [ ] Issue #71: Nested queries and mutations <!-- https://github.com/ash-project/ash_graphql/issues/71 -->
- [ ] Issue #57: Allow customization of key `result(s)` with mutations <!-- https://github.com/ash-project/ash_graphql/issues/57 -->
- [ ] Issue #55: Make sort/filter types optional and/or customizable <!-- https://github.com/ash-project/ash_graphql/issues/55 -->
- [ ] Issue #41: Catch if there are any apis missing from schema at compile time and provide instructions to add them to the list <!-- https://github.com/ash-project/ash_graphql/issues/41 -->

---

## ash_paper_trail (6 PRs, 22 issues)

### PRs
- [ ] PR #227: chore: SPDX contributor links <!-- https://github.com/ash-project/ash_paper_trail/pull/227 -->
- [ ] PR #226: chore(deps): bump ash from 3.11.3 to 3.13.2 in the production-dependencies group <!-- https://github.com/ash-project/ash_paper_trail/pull/226 -->
- [ ] PR #225: chore(deps-dev): bump the dev-dependencies group with 3 updates <!-- https://github.com/ash-project/ash_paper_trail/pull/225 -->
- [ ] PR #222: test: improve version sorting consistency across tests - addresses #2… <!-- https://github.com/ash-project/ash_paper_trail/pull/222 -->
- [ ] PR #220: Fix/version resource naming <!-- https://github.com/ash-project/ash_paper_trail/pull/220 -->
- [ ] PR #219: fix: handle ForbiddenField in full_diff when using field_policies (#215) <!-- https://github.com/ash-project/ash_paper_trail/pull/219 -->

### Issues
- [ ] Issue #221: Non-deterministic behavior in test suite <!-- https://github.com/ash-project/ash_paper_trail/issues/221 -->
- [ ] Issue #215: `full_diff` crashes on `%Ash.ForbiddenField{}` <!-- https://github.com/ash-project/ash_paper_trail/issues/215 -->
- [ ] Issue #209: Support composite primary keys <!-- https://github.com/ash-project/ash_paper_trail/issues/209 -->
- [ ] Issue #203: Add option to control `ash_paper_trail` changes tracing in runtime <!-- https://github.com/ash-project/ash_paper_trail/issues/203 -->
- [ ] Issue #202: Add an option to specify a name for the generated `Version` resource <!-- https://github.com/ash-project/ash_paper_trail/issues/202 -->
- [ ] Issue #171: AshPaperTrail Errors with Managed Relationships (?) <!-- https://github.com/ash-project/ash_paper_trail/issues/171 -->
- [ ] Issue #105: Option to set the name of the version relationship <!-- https://github.com/ash-project/ash_paper_trail/issues/105 -->
- [ ] Issue #104: Add support for non-referenced actors. <!-- https://github.com/ash-project/ash_paper_trail/issues/104 -->
- [ ] Issue #66: Error with missing `changed?` in context with `bulk_update` <!-- https://github.com/ash-project/ash_paper_trail/issues/66 -->
- [ ] Issue #52: feat: support atomic bulk actions <!-- https://github.com/ash-project/ash_paper_trail/issues/52 -->
- [ ] Issue #51: feat: allow adding metadata to versions <!-- https://github.com/ash-project/ash_paper_trail/issues/51 -->
- [ ] Issue #40: Support bulk action <!-- https://github.com/ash-project/ash_paper_trail/issues/40 -->
- [ ] Issue #39: Flaky test <!-- https://github.com/ash-project/ash_paper_trail/issues/39 -->
- [ ] Issue #30: When storing `:inputs` and `:changes` scrub them to redact `sensitive` values <!-- https://github.com/ash-project/ash_paper_trail/issues/30 -->
- [ ] Issue #21: Implement a behavior for custom types to control how they are diffed <!-- https://github.com/ash-project/ash_paper_trail/issues/21 -->
- [ ] Issue #20: Support storing inputs in addition to changes <!-- https://github.com/ash-project/ash_paper_trail/issues/20 -->
- [ ] Issue #19: Support custom change_tracking_mode via an mfa <!-- https://github.com/ash-project/ash_paper_trail/issues/19 -->
- [ ] Issue #17: Add support for non-resource actors <!-- https://github.com/ash-project/ash_paper_trail/issues/17 -->
- [ ] Issue #15: Remove mixin support, allow extension on the Version instead <!-- https://github.com/ash-project/ash_paper_trail/issues/15 -->
- [ ] Issue #13: Version policies <!-- https://github.com/ash-project/ash_paper_trail/issues/13 -->
- [ ] Issue #12: Add restore and revert actions dependent on diff change tracking <!-- https://github.com/ash-project/ash_paper_trail/issues/12 -->
- [ ] Issue #11: Allow changes_only tracking of embedded resources <!-- https://github.com/ash-project/ash_paper_trail/issues/11 -->

---

## igniter (3 PRs, 26 issues)

### PRs
- [ ] PR #291: feat: add assert_creates_file/3 macro to Igniter.Test <!-- https://github.com/ash-project/igniter/pull/291 -->
- [ ] PR #267: improvement: within tests and docs <!-- https://github.com/ash-project/igniter/pull/267 -->
- [ ] PR #237: Fix broken Map.get in Module.find_module <!-- https://github.com/ash-project/igniter/pull/237 -->

### Issues
- [ ] Issue #357: Elixir 1.20: unused require <!-- https://github.com/ash-project/igniter/issues/357 -->
- [ ] Issue #353: Add `--except` option to `igniter.upgrade` <!-- https://github.com/ash-project/igniter/issues/353 -->
- [ ] Issue #338: In "Desired/Found", using `quote` inside `add_dep` is printing the AST <!-- https://github.com/ash-project/igniter/issues/338 -->
- [ ] Issue #316: Allow specfying pattern where to include config line <!-- https://github.com/ash-project/igniter/issues/316 -->
- [ ] Issue #315: Support commented out config lines <!-- https://github.com/ash-project/igniter/issues/315 -->
- [ ] Issue #314: Igniter.Project.Config.configure/6 should do nothing when updater returns :error <!-- https://github.com/ash-project/igniter/issues/314 -->
- [ ] Issue #295: Document igniter.init_library in library author docs <!-- https://github.com/ash-project/igniter/issues/295 -->
- [ ] Issue #235: Igniter expects the definition body of Mix.Project#project to be exactly a keywordlist. <!-- https://github.com/ash-project/igniter/issues/235 -->
- [ ] Issue #225: Add `--include-git` to `mix igniter.upgrade --all` <!-- https://github.com/ash-project/igniter/issues/225 -->
- [ ] Issue #215: Proposal: adding `Igniter.copy` <!-- https://github.com/ash-project/igniter/issues/215 -->
- [ ] Issue #202: Diff output is not working correctly on Windows <!-- https://github.com/ash-project/igniter/issues/202 -->
- [ ] Issue #198: False message on `igniter.install` task <!-- https://github.com/ash-project/igniter/issues/198 -->
- [ ] Issue #195: `mix igniter.new` doesn't support `--help` directly. <!-- https://github.com/ash-project/igniter/issues/195 -->
- [ ] Issue #194: `Igniter.Project.Config.configure/6` doesn't update nested Keyword <!-- https://github.com/ash-project/igniter/issues/194 -->
- [ ] Issue #187: Performance Investigation: Speed Issues in `copy_template` Function <!-- https://github.com/ash-project/igniter/issues/187 -->
- [ ] Issue #182: Reserves `--help` for each mix task and showing bad args error with its helper <!-- https://github.com/ash-project/igniter/issues/182 -->
- [ ] Issue #171: Proposal: Deprecate `{:ok, {:code, Macro.t()}}` in favor of alternative form <!-- https://github.com/ash-project/igniter/issues/171 -->
- [ ] Issue #163: Support to update a version manually in a mix.exs file <!-- https://github.com/ash-project/igniter/issues/163 -->
- [ ] Issue #152: Send errors to stderr instead of stdio <!-- https://github.com/ash-project/igniter/issues/152 -->
- [ ] Issue #138: Proposal: plain mix task generator <!-- https://github.com/ash-project/igniter/issues/138 -->
- [ ] Issue #127: Proposal: `Igniter.CLI` for expanded interactivity support <!-- https://github.com/ash-project/igniter/issues/127 -->
- [ ] Issue #125: Confirm that the user is installing the correct package before adding it to the mix.exs file <!-- https://github.com/ash-project/igniter/issues/125 -->
- [ ] Issue #115: `Igniter.Refactors.Rename.rename_function/3` should update the `@spec` associated with the definition <!-- https://github.com/ash-project/igniter/issues/115 -->
- [ ] Issue #67: Add `Igniter.Project.Config.remove_configuration` <!-- https://github.com/ash-project/igniter/issues/67 -->
- [ ] Issue #66: Support not running installers if a package is already present <!-- https://github.com/ash-project/igniter/issues/66 -->
- [ ] Issue #29: Options for ignore files when moving and for configuring module locations <!-- https://github.com/ash-project/igniter/issues/29 -->

---

## ash_postgres (9 PRs, 37 issues)

### PRs
- [ ] PR #698: fix(Aggregate Uniq Count):  <!-- https://github.com/ash-project/ash_postgres/pull/698 -->
- [ ] PR #694: Fixing the varchar size migration bug (Issue #150) <!-- https://github.com/ash-project/ash_postgres/pull/694 -->
- [ ] PR #686: feat: relationship through <!-- https://github.com/ash-project/ash_postgres/pull/686 -->
- [ ] PR #685: Fix concurrent index migrations with schema-driven multitenancy #610 <!-- https://github.com/ash-project/ash_postgres/pull/685 -->
- [ ] PR #668: feat: add tests for is_distinct_from operators <!-- https://github.com/ash-project/ash_postgres/pull/668 -->
- [ ] PR #643: fix: tenant migrations should honor module attributes <!-- https://github.com/ash-project/ash_postgres/pull/643 -->
- [ ] PR #579: raise error if the constraint is created on a field that doesn't exist <!-- https://github.com/ash-project/ash_postgres/pull/579 -->
- [ ] PR #558: Docs: multitenancy code example <!-- https://github.com/ash-project/ash_postgres/pull/558 -->
- [ ] PR #535: test: multitenancy aggregate validation tests <!-- https://github.com/ash-project/ash_postgres/pull/535 -->

### Issues
- [ ] Issue #672: detect postgres v18+, to set different defaults and extensions <!-- https://github.com/ash-project/ash_postgres/issues/672 -->
- [ ] Issue #650: Config for setting collation for attributes <!-- https://github.com/ash-project/ash_postgres/issues/650 -->
- [ ] Issue #642: Cant rollback migration for tenants <!-- https://github.com/ash-project/ash_postgres/issues/642 -->
- [ ] Issue #628: Find a cleaner solution for the return_skipped_upsert? logic introduced in #626 <!-- https://github.com/ash-project/ash_postgres/issues/628 -->
- [ ] Issue #611: Unneeded Change Added to Migration <!-- https://github.com/ash-project/ash_postgres/issues/611 -->
- [ ] Issue #610: Creating indexes concurrently fails for schema-driven multi-tenancy <!-- https://github.com/ash-project/ash_postgres/issues/610 -->
- [ ] Issue #595: Generated migrations are sometimes non-rollback-able <!-- https://github.com/ash-project/ash_postgres/issues/595 -->
- [ ] Issue #587: Combination queries and loads issue <!-- https://github.com/ash-project/ash_postgres/issues/587 -->
- [ ] Issue #585: unable to generate migration manually after generating domain from tables <!-- https://github.com/ash-project/ash_postgres/issues/585 -->
- [ ] Issue #584: Removing `:context` multitenancy generates invalid migrations <!-- https://github.com/ash-project/ash_postgres/issues/584 -->
- [ ] Issue #553: support for Duration <!-- https://github.com/ash-project/ash_postgres/issues/553 -->
- [ ] Issue #538: PostgreSQL types in context based multi-tenancy <!-- https://github.com/ash-project/ash_postgres/issues/538 -->
- [ ] Issue #527: `ash_postgres.generate_migrations` not `dumping_to_native` when generating a default value <!-- https://github.com/ash-project/ash_postgres/issues/527 -->
- [ ] Issue #490: Add mix task to undo latest codegen/delete latest migration <!-- https://github.com/ash-project/ash_postgres/issues/490 -->
- [ ] Issue #462: Possible unsafe migrations warning <!-- https://github.com/ash-project/ash_postgres/issues/462 -->
- [ ] Issue #458: Support for `parent(parent(field))` <!-- https://github.com/ash-project/ash_postgres/issues/458 -->
- [ ] Issue #448: Specified timeout for an Ash action does not override the global timeout <!-- https://github.com/ash-project/ash_postgres/issues/448 -->
- [ ] Issue #437: Add ability to generate resources from existing postgres as fragments <!-- https://github.com/ash-project/ash_postgres/issues/437 -->
- [ ] Issue #423: Using references wrong does not generate error <!-- https://github.com/ash-project/ash_postgres/issues/423 -->
- [ ] Issue #383: Add a way to persist a calculation for a resource <!-- https://github.com/ash-project/ash_postgres/issues/383 -->
- [ ] Issue #362: Error when installing with Igniter - `Igniter.Project.Application.app_name/0 is undefined` <!-- https://github.com/ash-project/ash_postgres/issues/362 -->
- [ ] Issue #264: Switch to using `timestamp with time zone` by default <!-- https://github.com/ash-project/ash_postgres/issues/264 -->
- [ ] Issue #261: Add `ash_required/1` function in the `ash-functions` extension, and a mirrored `required!` function in ash expression syntax <!-- https://github.com/ash-project/ash_postgres/issues/261 -->
- [ ] Issue #239: `has_one` with `no_attributes? true` and `from_many? true` does not apply limiting to the query <!-- https://github.com/ash-project/ash_postgres/issues/239 -->
- [ ] Issue #236: Migration script places unique index below relationships they're needed for <!-- https://github.com/ash-project/ash_postgres/issues/236 -->
- [ ] Issue #233: Support `IDENTITY` instead of serial columns <!-- https://github.com/ash-project/ash_postgres/issues/233 -->
- [ ] Issue #224: drop tables on resources being missing <!-- https://github.com/ash-project/ash_postgres/issues/224 -->
- [ ] Issue #192: Relationships are able to cross tenants with attribute strategy <!-- https://github.com/ash-project/ash_postgres/issues/192 -->
- [ ] Issue #189: polymorphic_on_delete option needs refining and potentially moving <!-- https://github.com/ash-project/ash_postgres/issues/189 -->
- [ ] Issue #169: Using an AshPostgres resource with a Timescale Hypertable. <!-- https://github.com/ash-project/ash_postgres/issues/169 -->
- [ ] Issue #159: Wrong migration order for unique_index when renaming field <!-- https://github.com/ash-project/ash_postgres/issues/159 -->
- [ ] Issue #150: varchar size info for a column that already exists is missing from migration <!-- https://github.com/ash-project/ash_postgres/issues/150 -->
- [ ] Issue #145: Mix task `ash_postgres.generate_migrations` doesn't say anything about `ash_apis` applicatione environment. <!-- https://github.com/ash-project/ash_postgres/issues/145 -->
- [ ] Issue #142: Warn on console when a generated migration contains reviewable steps <!-- https://github.com/ash-project/ash_postgres/issues/142 -->
- [ ] Issue #141: Proxy support for the multitenancy context strategy (pgbouncer and Supavisor) <!-- https://github.com/ash-project/ash_postgres/issues/141 -->
- [ ] Issue #135: Ash functions duplicated in migration <!-- https://github.com/ash-project/ash_postgres/issues/135 -->
- [ ] Issue #75: Redesign "snapshots" to be "operations" <!-- https://github.com/ash-project/ash_postgres/issues/75 -->

---

## ash (9 PRs, 128 issues)

### PRs
- [ ] PR #2581: Include usage-rules directory in package <!-- https://github.com/ash-project/ash/pull/2581 -->
- [ ] PR #2579: fix: boolean expression not eq optimization <!-- https://github.com/ash-project/ash/pull/2579 -->
- [ ] PR #2578: Enhance Ash.Type.NewType custom constraints to support arrays <!-- https://github.com/ash-project/ash/pull/2578 -->
- [ ] PR #2576: feat: add support for data layers with partial success in bulk_create <!-- https://github.com/ash-project/ash/pull/2576 -->
- [ ] PR #2573: #2420 error logs + tests <!-- https://github.com/ash-project/ash/pull/2573 -->
- [ ] PR #2570: Add error messages <!-- https://github.com/ash-project/ash/pull/2570 -->
- [ ] PR #2568: Support multiple filters in aggregations <!-- https://github.com/ash-project/ash/pull/2568 -->
- [ ] PR #2567: feat: Relationship Through <!-- https://github.com/ash-project/ash/pull/2567 -->
- [ ] PR #2566: build(deps): bump step-security/harden-runner from 2.14.1 to 2.14.2 in the github-actions group <!-- https://github.com/ash-project/ash/pull/2566 -->

### Issues
- [ ] Issue #2580: Manual Relationship Should select all <!-- https://github.com/ash-project/ash/issues/2580 -->
- [ ] Issue #2577: `no_attributes? true` relationship with multiple `parent()` refs in `or` filter produces cartesian product <!-- https://github.com/ash-project/ash/issues/2577 -->
- [ ] Issue #2569: Inconsistent option `error?` vs `not_found_error?` for `Ash.get()` vs `get? true` in action? <!-- https://github.com/ash-project/ash/issues/2569 -->
- [ ] Issue #2514: Pagination on dynamic (manual) relationship fails when queried <!-- https://github.com/ash-project/ash/issues/2514 -->
- [ ] Issue #2504: mix igniter.install ash_authentication_phoenix with --user/--token creates an unusable app <!-- https://github.com/ash-project/ash/issues/2504 -->
- [ ] Issue #2472: Batch validations <!-- https://github.com/ash-project/ash/issues/2472 -->
- [ ] Issue #2463: Unable to use arbitrary uuid versions with Ash.Type.UUID <!-- https://github.com/ash-project/ash/issues/2463 -->
- [ ] Issue #2432: Add support for aggregated policy evaluator Ash.can_do_all?([{...}] <!-- https://github.com/ash-project/ash/issues/2432 -->
- [ ] Issue #2420: More (user-) fault tolerant install script <!-- https://github.com/ash-project/ash/issues/2420 -->
- [ ] Issue #2406: Policies should have an `applicable_action_types` callback <!-- https://github.com/ash-project/ash/issues/2406 -->
- [ ] Issue #2392: Static defaults for array embedded resource attributes cause a cryptic compile error. <!-- https://github.com/ash-project/ash/issues/2392 -->
- [ ] Issue #2360: Auto-detect whether fields from validation errors are attributes or arguments <!-- https://github.com/ash-project/ash/issues/2360 -->
- [ ] Issue #2331: Improving error messages when embedded resources have validation issues <!-- https://github.com/ash-project/ash/issues/2331 -->
- [ ] Issue #2295: Can no longer call `typed_struct` multiple times <!-- https://github.com/ash-project/ash/issues/2295 -->
- [ ] Issue #2278: Duplicate extension migrations when using multiple repos <!-- https://github.com/ash-project/ash/issues/2278 -->
- [ ] Issue #2275: Policies for updates can generate invalid ecto code when loading other resources <!-- https://github.com/ash-project/ash/issues/2275 -->
- [ ] Issue #2274: Using `exists()` in policy with syntax error produces very obtuse stack trace <!-- https://github.com/ash-project/ash/issues/2274 -->
- [ ] Issue #2267: Initiative: Reduce compilation time <!-- https://github.com/ash-project/ash/issues/2267 -->
- [ ] Issue #2255: Docs improvement: Testing with references to DataCase & DBConnection.OwnershipError <!-- https://github.com/ash-project/ash/issues/2255 -->
- [ ] Issue #2242: Calculations: Add `field?: false` option to exclude from Resource struct fields <!-- https://github.com/ash-project/ash/issues/2242 -->
- [ ] Issue #2207: Code interface default loads could unwrap nested lists (or provide more user-friendly error message) <!-- https://github.com/ash-project/ash/issues/2207 -->
- [ ] Issue #2174: before_action hooks cannot add loads or calculations during read actions <!-- https://github.com/ash-project/ash/issues/2174 -->
- [ ] Issue #2099: Proposal: Allow for enforcning minimum bulk strategy on actions <!-- https://github.com/ash-project/ash/issues/2099 -->
- [ ] Issue #2081: Proposal: Use relationships in PubSub topics to fan out <!-- https://github.com/ash-project/ash/issues/2081 -->
- [ ] Issue #2057: Ash should raise an error if an actor template expands to not loaded <!-- https://github.com/ash-project/ash/issues/2057 -->
- [ ] Issue #2015: Require `opts` to be passed to `define`’d code interface <!-- https://github.com/ash-project/ash/issues/2015 -->
- [ ] Issue #1980: Extension Proposal: Ash Billing <!-- https://github.com/ash-project/ash/issues/1980 -->
- [ ] Issue #1971: Defining `get_by` on a code interface doesn't do type-checking of the argument <!-- https://github.com/ash-project/ash/issues/1971 -->
- [ ] Issue #1937: `allow_nil?` and it's default <!-- https://github.com/ash-project/ash/issues/1937 -->
- [ ] Issue #1932: changes block and change in action differ from one another <!-- https://github.com/ash-project/ash/issues/1932 -->
- [ ] Issue #1907: two "mix ash_phoenix.gen.live" commands for the same resource create assets in two directories <!-- https://github.com/ash-project/ash/issues/1907 -->
- [ ] Issue #1899: Ash Custom Types Constraints Validations <!-- https://github.com/ash-project/ash/issues/1899 -->
- [ ] Issue #1886: Add `get` step to `Ash.Reactor` <!-- https://github.com/ash-project/ash/issues/1886 -->
- [ ] Issue #1879: Redact sensitive values in built in validation error <!-- https://github.com/ash-project/ash/issues/1879 -->
- [ ] Issue #1877: Standardize type for returning extra notifications from hook functions and manual actions <!-- https://github.com/ash-project/ash/issues/1877 -->
- [ ] Issue #1865: Expr produced invalid SQL instead of ash-error <!-- https://github.com/ash-project/ash/issues/1865 -->
- [ ] Issue #1830: Proposal (4.0): Inline global preparations into actions at compile time <!-- https://github.com/ash-project/ash/issues/1830 -->
- [ ] Issue #1811: Possible bug in `Ash.Actions.Update.Bulk.do_run/3` <!-- https://github.com/ash-project/ash/issues/1811 -->
- [ ] Issue #1792: Proposal(4.0): Make adding a `transform` required when defining a pubsub notification <!-- https://github.com/ash-project/ash/issues/1792 -->
- [ ] Issue #1772: Add in required validation, replicating Ecto's validate_required functionality <!-- https://github.com/ash-project/ash/issues/1772 -->
- [ ] Issue #1771: Generate gettext file for various error messages <!-- https://github.com/ash-project/ash/issues/1771 -->
- [ ] Issue #1747: Complex & Multi step actions guide <!-- https://github.com/ash-project/ash/issues/1747 -->
- [ ] Issue #1740: Add offet / limit to has_many and has_one relationships <!-- https://github.com/ash-project/ash/issues/1740 -->
- [ ] Issue #1733: ManualRead escape-hatch isn't deep enough; pk's are captured inside Ash.Filter <!-- https://github.com/ash-project/ash/issues/1733 -->
- [ ] Issue #1713: proposal: attribute_in should be alias of one_of to simplify api <!-- https://github.com/ash-project/ash/issues/1713 -->
- [ ] Issue #1696: Code Interface: Allow defining `?` functions that return the unwrapped boolean result <!-- https://github.com/ash-project/ash/issues/1696 -->
- [ ] Issue #1675: Add a plug that detects the need for running `mix ash.codegen` <!-- https://github.com/ash-project/ash/issues/1675 -->
- [ ] Issue #1660: Support destroy and update actions on resources that have no primary key <!-- https://github.com/ash-project/ash/issues/1660 -->
- [ ] Issue #1649: Assumption failed: got a query without a domain <!-- https://github.com/ash-project/ash/issues/1649 -->
- [ ] Issue #1646: 4.0: Incosistencies of `public?` for fields and for arguments and its effect on `accept` <!-- https://github.com/ash-project/ash/issues/1646 -->
- [ ] Issue #1637: Incompatible type coercion/casting in main-branch, breaking ash_money <!-- https://github.com/ash-project/ash/issues/1637 -->
- [ ] Issue #1628: 4.0: Return a single record from `Ash.read` when the action has `get?: true` <!-- https://github.com/ash-project/ash/issues/1628 -->
- [ ] Issue #1627: 4.0: reverse conditions in the `atomic` callback of 4.0 <!-- https://github.com/ash-project/ash/issues/1627 -->
- [ ] Issue #1626: Support expressions as the body of a validation <!-- https://github.com/ash-project/ash/issues/1626 -->
- [ ] Issue #1625: Support pipelines, to declare subsections of action logic that can be referenced from many other places <!-- https://github.com/ash-project/ash/issues/1625 -->
- [ ] Issue #1621: Add `override_type_constraints` for union <!-- https://github.com/ash-project/ash/issues/1621 -->
- [ ] Issue #1606: This should not be possible, please report a detailed bug at error when error is very clear <!-- https://github.com/ash-project/ash/issues/1606 -->
- [ ] Issue #1595: ash.gen.resource warning can not add to resource list <!-- https://github.com/ash-project/ash/issues/1595 -->
- [ ] Issue #1592: `:authorize_with: :error` is not available with `Ash.get/3` <!-- https://github.com/ash-project/ash/issues/1592 -->
- [ ] Issue #1581: Use bulk actions to optimize `manage_relationship` <!-- https://github.com/ash-project/ash/issues/1581 -->
- [ ] Issue #1580: Add before_batch_transaction and after_batch_transaction callbacks to `Ash.Resource.Change` <!-- https://github.com/ash-project/ash/issues/1580 -->
- [ ] Issue #1579: Support validations on read actions <!-- https://github.com/ash-project/ash/issues/1579 -->
- [ ] Issue #1578: Warn on calling actions inside of the change body &  not a hook <!-- https://github.com/ash-project/ash/issues/1578 -->
- [ ] Issue #1543: Custom message in individual policy <!-- https://github.com/ash-project/ash/issues/1543 -->
- [ ] Issue #1537: misleading error message with multiple "use Ash.Domain" macro invocations. <!-- https://github.com/ash-project/ash/issues/1537 -->
- [ ] Issue #1514: Allow reactor modules to be entered directy as manual action implemenations. <!-- https://github.com/ash-project/ash/issues/1514 -->
- [ ] Issue #1509: Overhaul Upserts in 4.x <!-- https://github.com/ash-project/ash/issues/1509 -->
- [ ] Issue #1507: Enhance set_attribute to enable attribute copying <!-- https://github.com/ash-project/ash/issues/1507 -->
- [ ] Issue #1504: Compile-time checks for validations like `attribute_equals`, that verify the first arg is actually an attribute on the resource <!-- https://github.com/ash-project/ash/issues/1504 -->
- [ ] Issue #1498: Handle Notifications when running an action inside an action <!-- https://github.com/ash-project/ash/issues/1498 -->
- [ ] Issue #1486: 4.0: Deprecate `accept [:*]` and remove from docs <!-- https://github.com/ash-project/ash/issues/1486 -->
- [ ] Issue #1468: Missing Read on Join Resource for many_to_many complains about destination resource missing read action <!-- https://github.com/ash-project/ash/issues/1468 -->
- [ ] Issue #1444: performance: expression calculations are hydrated 3 times <!-- https://github.com/ash-project/ash/issues/1444 -->
- [ ] Issue #1426: Likely bugs detected by mistyped calls to Info.calculation/relationship <!-- https://github.com/ash-project/ash/issues/1426 -->
- [ ] Issue #1394: `ash.codegen` generates wrong sequence of migration <!-- https://github.com/ash-project/ash/issues/1394 -->
- [ ] Issue #1383: nested bulk_create failing when using transaction: :batch <!-- https://github.com/ash-project/ash/issues/1383 -->
- [ ] Issue #1367: Possibility to emit indices in bulk action with `return_stream?: true` <!-- https://github.com/ash-project/ash/issues/1367 -->
- [ ] Issue #1351: Show an error if an argument defined in `define_calculation` is not accepted in the calculation itself <!-- https://github.com/ash-project/ash/issues/1351 -->
- [ ] Issue #1350: Add `filter` option to update/destroy actions <!-- https://github.com/ash-project/ash/issues/1350 -->
- [ ] Issue #1303: feat: add uuid_generate_v7/1, accepting a timestamp arg <!-- https://github.com/ash-project/ash/issues/1303 -->
- [ ] Issue #1295: `mix ash.patch.extract_functions` <!-- https://github.com/ash-project/ash/issues/1295 -->
- [ ] Issue #1294: Add a `multi_error` function that takes a variable list of conditions, modules, and payloads <!-- https://github.com/ash-project/ash/issues/1294 -->
- [ ] Issue #1286: Reenable filter predicate analysis prior to authorization <!-- https://github.com/ash-project/ash/issues/1286 -->
- [ ] Issue #1285: Add `access_type :query_error` to policies. <!-- https://github.com/ash-project/ash/issues/1285 -->
- [ ] Issue #1282: Optimize calculation/aggregate/relationship merging/reuse in calculation dependencies <!-- https://github.com/ash-project/ash/issues/1282 -->
- [ ] Issue #1271: Allow marking actions as internal to prevent exposing them via APIs <!-- https://github.com/ash-project/ash/issues/1271 -->
- [ ] Issue #1265: Support multiple filters in aggregations <!-- https://github.com/ash-project/ash/issues/1265 -->
- [ ] Issue #1246: Modify `manage_relationship` to handle destroy first, or add an option to prioritize it <!-- https://github.com/ash-project/ash/issues/1246 -->
- [ ] Issue #1244: In Ash.Query.build, load of relationship obliterates select on that relationship <!-- https://github.com/ash-project/ash/issues/1244 -->
- [ ] Issue #1233: Not selecting an embed makes query blow up <!-- https://github.com/ash-project/ash/issues/1233 -->
- [ ] Issue #1224: In Ash.can?, check if tenant given matches the tenant in the provided record <!-- https://github.com/ash-project/ash/issues/1224 -->
- [ ] Issue #1202: PubSub DSL module option doesn't expand aliases. <!-- https://github.com/ash-project/ash/issues/1202 -->
- [ ] Issue #1193: Don't run queries with `filter: false` <!-- https://github.com/ash-project/ash/issues/1193 -->
- [ ] Issue #1170: Better documentation/examples for `lazy?` and `reuse_values?` <!-- https://github.com/ash-project/ash/issues/1170 -->
- [ ] Issue #1169: Validate return types of all behavior callbacks <!-- https://github.com/ash-project/ash/issues/1169 -->
- [ ] Issue #1132: Support loading through aggregates <!-- https://github.com/ash-project/ash/issues/1132 -->
- [ ] Issue #1131: Optimization: `set_source` and `group_by_source` callbacks on types that support loading through <!-- https://github.com/ash-project/ash/issues/1131 -->
- [ ] Issue #1118: Add `init(opts)` callback to `Ash.Policy.Check` <!-- https://github.com/ash-project/ash/issues/1118 -->
- [ ] Issue #1092: Non-macro alternative to `Ash.Query.filter` <!-- https://github.com/ash-project/ash/issues/1092 -->
- [ ] Issue #1074: Add domain-less eager checking also to `Ash.Changeset.manage_relationship/4` <!-- https://github.com/ash-project/ash/issues/1074 -->
- [ ] Issue #1071: Add an additional error class to expose application and/or temporary errors <!-- https://github.com/ash-project/ash/issues/1071 -->
- [ ] Issue #1069: Add `%Ash.Expr{}`, which will be returned from `expr/1` <!-- https://github.com/ash-project/ash/issues/1069 -->
- [ ] Issue #1052: Proposal: Create Ash module to deal with `file` resources / uploads. <!-- https://github.com/ash-project/ash/issues/1052 -->
- [ ] Issue #992: Calling a code interface create with keyword as input instead of a map gives a non-helpful failure message. <!-- https://github.com/ash-project/ash/issues/992 -->
- [ ] Issue #966: Add ability to extend actions <!-- https://github.com/ash-project/ash/issues/966 -->
- [ ] Issue #918: Support methods around field policies <!-- https://github.com/ash-project/ash/issues/918 -->
- [ ] Issue #885: Add more metadata to `Ash.NotLoaded` and `Ash.NotSelected`. <!-- https://github.com/ash-project/ash/issues/885 -->
- [ ] Issue #860: Using custom types on embedded resources throws an error <!-- https://github.com/ash-project/ash/issues/860 -->
- [ ] Issue #859: Many to many and on match destroy in `manage_relationship` <!-- https://github.com/ash-project/ash/issues/859 -->
- [ ] Issue #799: Calculations with `allow_nil?: false` should raise an error when loading `nil` value <!-- https://github.com/ash-project/ash/issues/799 -->
- [ ] Issue #791: Typo in datalayer produces somewhat cryptic error messages (and compile deadlocks if using extensions) <!-- https://github.com/ash-project/ash/issues/791 -->
- [ ] Issue #741: Generate forms for resource actions in Livebook <!-- https://github.com/ash-project/ash/issues/741 -->
- [ ] Issue #729: Allow to read applying another non-read policy <!-- https://github.com/ash-project/ash/issues/729 -->
- [ ] Issue #716: Compiler doesn't warn when using Ash.Policy.Authorizer in wrong place. <!-- https://github.com/ash-project/ash/issues/716 -->
- [ ] Issue #711: Allow to specify a read action for a relationship in a load <!-- https://github.com/ash-project/ash/issues/711 -->
- [ ] Issue #708: Ash.Resource does not translate into Ecto.Schema with correct autogenerate <!-- https://github.com/ash-project/ash/issues/708 -->
- [ ] Issue #622: `PubSub` notifier should load any data it needs <!-- https://github.com/ash-project/ash/issues/622 -->
- [ ] Issue #593: `literal/1` for expanding literal values in fragments <!-- https://github.com/ash-project/ash/issues/593 -->
- [ ] Issue #592: Ash should automatically define join resources for many-to-many relationships. <!-- https://github.com/ash-project/ash/issues/592 -->
- [ ] Issue #577: Code Generation <!-- https://github.com/ash-project/ash/issues/577 -->
- [ ] Issue #563: Polymorphic relationships <!-- https://github.com/ash-project/ash/issues/563 -->
- [ ] Issue #560: Synthesizing Aggregates across data layer boundaries <!-- https://github.com/ash-project/ash/issues/560 -->
- [ ] Issue #478: Support using calculations as relationship keys <!-- https://github.com/ash-project/ash/issues/478 -->
- [ ] Issue #365: Support error messages in `cast_stored` and `dump_to_native`. <!-- https://github.com/ash-project/ash/issues/365 -->
- [ ] Issue #209: When using an `on_match: ...` we should only look up matching records if there is no `on_missing`, potentially. <!-- https://github.com/ash-project/ash/issues/209 -->
- [ ] Issue #72: Support Through Relationships <!-- https://github.com/ash-project/ash/issues/72 -->
- [ ] Issue #355: Support lazy actor evaluation <!-- https://github.com/ash-project/ash/issues/355 -->
- [ ] Issue #49: Extension Proposal: ash_twirp <!-- https://github.com/ash-project/ash/issues/49 -->

