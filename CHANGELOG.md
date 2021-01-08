# Change Log

All notable changes to this project will be documented in this file.
See [Conventional Commits](Https://conventionalcommits.org) for commit guidelines.

<!-- changelog -->

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

* Optimize relashionship records replacement (#135)

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
