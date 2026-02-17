<!--
SPDX-FileCopyrightText: 2023 James Harton, Zach Daniel, Alembic Pty and contributors
SPDX-FileCopyrightText: 2023 reactor contributors <https://github.com/ash-project/reactor/graphs.contributors>

SPDX-License-Identifier: MIT
-->

# Change Log

All notable changes to this project will be documented in this file.
See [Conventional Commits](Https://conventionalcommits.org) for commit guidelines.

<!-- changelog -->

## [v1.0.0](https://github.com/ash-project/reactor/compare/v0.17.0...v1.0.0) (2026-01-25)
### Breaking Changes:

* Store inputs in their own struct (#269) by James Harton



### Features:

* add tests for guard telemetry events by Jaden

### Bug Fixes:

* remove unused async options from `Reactor.undo/3` (#292) by James Harton

* move mermaid script to `before_closing_body_tag` for proper rendering (#289) by James Harton

* implement `nested_steps/1` for `Step.Switch` to fix dependency resolution (#288) by James Harton

* set proper class for reactor errors (#286) by Zach Daniel

* Telemetry middleware crashes on unhandled guard events (#276) by Jaden

* telemetry middleware implements guard steps by Jaden

* tests failing because of REUSE annotations (#275) by James Harton

## [v0.17.0](https://github.com/ash-project/reactor/compare/v0.16.0...v0.17.0) (2025-09-30)




### Features:

* Add backoff support for retries. (#267) by James Harton

### Bug Fixes:

* Add stacktrace to exception (#265) by Rutgerdj

* Add stacktrace to exception by Rutgerdj

* remove rescue statement inside AnonFn.run/3 by Rutgerdj

### Improvements:

* Add `description` option to Reactor DSL and struct. (#268) by James Harton

* Prepare Minimal Spark Update (#266) by Jonatan Männchen

## [v0.16.0](https://github.com/ash-project/reactor/compare/v0.15.6...v0.16.0) (2025-09-21)




### Features:

* Allow Reactors to be able to be undone after successful completion. (#262) by James Harton

* add allow_async? flag to compose DSL (#256) by James Harton

### Bug Fixes:

* undo for composed reactors (#263) by James Harton

* resolve nested step dependency and argument inheritance issues (#258) by James Harton

* correct current_try increment during step retries (#257) by James Harton

* correct current_try increment during step retries by James Harton

* mermaid: trim whitespace from module names to prevent :nofile errors (#253) by James Harton

## [v0.15.6](https://github.com/ash-project/reactor/compare/v0.15.5...v0.15.6) (2025-07-02)




### Bug Fixes:

* mermaid: use TB instead of TD (#241) by ChristianAlexander

## [v0.15.5](https://github.com/ash-project/reactor/compare/v0.15.4...v0.15.5) (2025-06-18)




### Bug Fixes:

* don't use regexes in module attributes, for otp28 compat by Zach Daniel

## [v0.15.4](https://github.com/ash-project/reactor/compare/v0.15.3...v0.15.4) (2025-05-30)




### Improvements:

* add support for recursive sub-reactors (#215)

* add support for recursive sub-reactors

## [v0.15.3](https://github.com/ash-project/reactor/compare/v0.15.2...v0.15.3) (2025-05-24)




### Improvements:

* add usage-rules.md (#218)

## [v0.15.2](https://github.com/ash-project/reactor/compare/v0.15.1...v0.15.2) (2025-04-15)




### Improvements:

* Include a unique `run_id` for every reactor run. (#201)

## [v0.15.1](https://github.com/ash-project/reactor/compare/v0.15.0...v0.15.1) (2025-04-09)




### Improvements:

* Allow additional telemetry metadata to be provided via Reactor context. (#194)

## [v0.15.0](https://github.com/ash-project/reactor/compare/v0.14.0...v0.15.0) (2025-03-11)




### Features:

* Add `reactor.run` mix task. (#184)

## [v0.14.0](https://github.com/ash-project/reactor/compare/v0.13.3...v0.14.0) (2025-03-04)




### Features:

* Add ability to convert Reactors into Mermaid flowcharts.

### Improvements:

* Add the ability to render Mermaid flowcharts. (#175)

* Ensure that descriptions are carried through into Reactor structs.

## [v0.13.3](https://github.com/ash-project/reactor/compare/v0.13.2...v0.13.3) (2025-02-17)




### Bug Fixes:

* pass full step to undo error

## [v0.13.2](https://github.com/ash-project/reactor/compare/v0.13.1...v0.13.2) (2025-02-15)




### Bug Fixes:

* context should be shared with child reactors by default. (#168)

## [v0.13.1](https://github.com/ash-project/reactor/compare/v0.13.0...v0.13.1) (2025-02-11)




### Bug Fixes:

* Radically simplify reactor composition. (#162)

## [v0.13.0](https://github.com/ash-project/reactor/compare/v0.12.1...v0.13.0) (2025-02-11)




### Features:

* template: Add `template` step which renders an EEx template. (#154)

* Add `where` and `guard` entities to steps. (#148)

### Bug Fixes:

* Correctly pass guards into `flunk` step. (#158)

* Runtime step planning with intermediate results. (#156)

### Improvements:

* Verify step name uniqueness recursively. (#153)

## [v0.12.1](https://github.com/ash-project/reactor/compare/v0.12.0...v0.12.1) (2025-02-04)




### Bug Fixes:

* Correctly pass guards into `flunk` step. (#158)

* Runtime step planning with intermediate results. (#156)

## [v0.12.0](https://github.com/ash-project/reactor/compare/v0.11.0...v0.12.0) (2025-02-01)




### Features:

* template: Add `template` step which renders an EEx template. (#154)

### Improvements:

* Verify step name uniqueness recursively. (#153)

## [v0.11.0](https://github.com/ash-project/reactor/compare/v0.10.3...v0.11.0) (2025-01-28)




### Features:

* Add `where` and `guard` entities to steps. (#148)

## [v0.10.3](https://github.com/ash-project/reactor/compare/v0.10.2...v0.10.3) (2024-12-19)




### Improvements:

* make igniter optional

## [v0.10.2](https://github.com/ash-project/reactor/compare/v0.10.1...v0.10.2) (2024-12-01)




### Improvements:

* DSL: Add `description` options to most DSL entities. (#137)

## [v0.10.1](https://github.com/ash-project/reactor/compare/v0.10.0...v0.10.1) (2024-11-06)




### Bug Fixes:

* Switch: Don't duplicate outside steps inside switch matches. (#133)

* Don't raise error when switch is missing default branch (#129)

## [v0.10.0](https://github.com/ash-project/reactor/compare/v0.9.1...v0.10.0) (2024-09-15)




### Features:

* Reactor.Dsl.Flunk: Add a special step type which always fails. (#125)

## [v0.9.1](https://github.com/ash-project/reactor/compare/v0.9.0...v0.9.1) (2024-08-12)




### Bug Fixes:

* `Reactor.run!/4` should not return an `:ok` tuple.

## [v0.9.0](https://github.com/ash-project/reactor/compare/v0.8.5...v0.9.0) (2024-07-18)




### Features:

* map: Add the ability to map over elements of a collection inside a reactor. (#123)

* map: Add the ability to map over elements of a collection inside a reactor.

### Bug Fixes:

* automatically pass extra arguments from the map step to nested steps.

* spurious test failures seemingly caused by `Mimic`.

### Improvements:

* throw a more helpful error when a step returns an invalid result.

## [v0.8.5](https://github.com/ash-project/reactor/compare/v0.8.4...v0.8.5) (2024-07-10)




### Improvements:

* add `mix reactor.install` (#124)

## [v0.8.4](https://github.com/ash-project/reactor/compare/v0.8.3...v0.8.4) (2024-05-25)




### Bug Fixes:

* inability to store composed reactors at compile time.

## [v0.8.3](https://github.com/ash-project/reactor/compare/v0.8.2...v0.8.3) (2024-05-24)




### Bug Fixes:

* Missing `__identifier__` field in `compose` DSL struct.

## [v0.8.2](https://github.com/ash-project/reactor/compare/v0.8.1...v0.8.2) (2024-05-08)




### Bug Fixes:

* initialisation issue with middlewares.

## [v0.8.1](https://github.com/ash-project/reactor/compare/v0.8.0...v0.8.1) (2024-03-20)




### Bug Fixes:

* RunStepError: pass entire step struct instead of just name when raising.

## [v0.8.0](https://github.com/ash-project/reactor/compare/v0.7.0...v0.8.0) (2024-03-18)
### Breaking Changes:

* Use `Splode` for managing errors. (#97)



### Bug Fixes:

* Don't assume `UndefinedFunctionError` means the module is not a Reactor.

### Improvements:

* Add template guards.

## [v0.7.0](https://github.com/ash-project/reactor/compare/v0.6.0...v0.7.0) (2024-02-28)




### Features:

* Add telemetry middleware. (#93)

* Add a middleware which emits telemetry events about Reactor.

### Bug Fixes:

* incorrect function arity for `Group.after_fun` DSL.

### Improvements:

* don't incur compile-time dependencies on middleware.

## [v0.6.0](https://github.com/ash-project/reactor/compare/v0.5.2...v0.6.0) (2024-02-26)
### Breaking Changes:

* Remove hooks and replace with middleware behaviour. (#90)

* Remove hooks and replace with middleware behaviour.



### Improvements:

* Middleware: Add `get_process_context/0` and `set_process_context/1` middleware hooks.

* Add step event callback to middleware.

## [v0.5.2](https://github.com/ash-project/reactor/compare/v0.5.1...v0.5.2) (2024-02-18)




### Bug Fixes:

* callback spec for `Reactor.Step.async?/1`.

### Performance Improvements:

* Don't iterate the entire graph every time through the loop. (#88)

## [v0.5.1](https://github.com/ash-project/reactor/compare/v0.5.0...v0.5.1) (2024-02-14)




### Improvements:

* Move `can?/2` and `async?/1` into `Reactor.Step` behaviour. (#87)

## [v0.5.0](https://github.com/ash-project/reactor/compare/v0.4.1...v0.5.0) (2024-02-07)




### Features:

* Add lifecycle hooks to Reactor (#83)

### Bug Fixes:

* don't deadlock when lots of async reactors are sharing a concurrency pool. (#36)

* weird issue with aliases sometimes not being expanded in generated reactors. (#58)

### Improvements:

* Add ability for steps to decide at runtime whether they should be run asyncronously. (#84)

## [v0.4.1](https://github.com/ash-project/reactor/compare/v0.4.0...v0.4.1) (2023-09-26)




### Bug Fixes:

* weird issue with aliases sometimes not being expanded in generated reactors.

## [v0.4.0](https://github.com/ash-project/reactor/compare/v0.3.5...v0.4.0) (2023-09-11)




### Features:

* Add `collect` step entity. (#53)

## [v0.3.5](https://github.com/ash-project/reactor/compare/v0.3.4...v0.3.5) (2023-09-06)




### Improvements:

* Template: Abstract template type so that it can be used by extensions.

## [v0.3.4](https://github.com/ash-project/reactor/compare/v0.3.3...v0.3.4) (2023-09-04)




### Bug Fixes:

* Allow `reactor` DSL section to be patched.

* Reactor: fix call to `use Spark.Dsl`.

## [v0.3.3](https://github.com/ash-project/reactor/compare/v0.3.2...v0.3.3) (2023-09-01)




### Improvements:

* Dsl: Extract DSL entities into their target modules. (#50)

## [v0.3.2](https://github.com/ash-project/reactor/compare/v0.3.1...v0.3.2) (2023-07-27)




### Bug Fixes:

* Don't swallow errors when a step runs out of retries. (#41)

## [v0.3.1](https://github.com/ash-project/reactor/compare/v0.3.0...v0.3.1) (2023-07-24)




### Improvements:

* Add `wait_for` DSL. (#39)

* Add "subpaths" to templates. (#31)

* Step.Debug: Add `debug` step and DSL. (#30)

* Step.Switch: Add `switch` DSL and step type. (#29)

## [v0.3.0](https://github.com/ash-project/reactor/compare/v0.2.4...v0.3.0) (2023-07-03)




### Features:

* Step.Around: Add ability to wrap a function around a group of steps. (#24)

### Bug Fixes:

* Executor: don't double-iterate the graph each time through the loop.

### Improvements:

* Add `group` DSL entity and `Reactor.Step.Group`. (#27)

* Reactor.Executor: track concurrent process usage across multiple reactors. (#26)

* Support `timeout` and `async?` Reactor options. (#20)

* Invert DSL entity building. (#19)

* Allow entire step behaviour to be defined in the DSL. (#18)

### Performance Improvements:

* Dsl: Build and pre-plan DSL reactors.

* Builder: build transformation steps as synchronous.

## [v0.2.4](https://github.com/ash-project/reactor/compare/v0.2.3...v0.2.4) (2023-06-15)




### Improvements:

* Add ability to compose reactors.

* Builder: rename internally generated steps to start with `:__reactor__`. (#10)

## [v0.2.3](https://github.com/ash-project/reactor/compare/v0.2.2...v0.2.3) (2023-06-07)




### Improvements:

* Add step-wide argument transforms. (#9)

* Add step-wide argument transforms.

## [v0.2.2](https://github.com/ash-project/reactor/compare/v0.2.1...v0.2.2) (2023-05-15)




### Bug Fixes:

* Reactor.Argument: Remove spurious `is_atom` constraint on `Argument.from_input/2..3`.

## [v0.2.1](https://github.com/ash-project/reactor/compare/v0.2.0...v0.2.1) (2023-05-12)




### Improvements:

* Reactor.Step: remove `can?/1` and replace with optional callbacks. (#6)

## [v0.2.0](https://github.com/ash-project/reactor/compare/v0.1.0...v0.2.0) (2023-05-10)




### Features:

* implement basic reactor behaviour. (#1)

## [v0.1.0](https://github.com/ash-project/reactor/compare/v0.1.0...v0.1.0) (2023-04-19)
