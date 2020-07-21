# Change Log

All notable changes to this project will be documented in this file.
See [Conventional Commits](Https://conventionalcommits.org) for commit guidelines.

<!-- changelog -->

## [v0.12.0](https://github.com/ash-project/ash/compare/0.11.0...v0.12.0) (2020-07-21)




### Features:

* rename package back to `ash`

## [v0.11.0](https://github.com/ash-project/ash/compare/0.11.0...v0.11.0) (2020-07-21)
### Breaking Changes:

* remove initial subscriptions pass

* remove atom type, add docs



### Features:

* allow editing join association attributes

* various small refactors + validations

* refactor changes into changesets

* lots of docs, simplify query generation

* validate relationship keys

* general improvements

* list types

* refactor ash types to modules, add constraints

* add less_than and greater_than filter support

* validate all related resources in API

* cross data layer filters

* cross data layer filtering

* section option configuration

* boolean filter refactor (#78)

* predicate behaviour

* extension section module imports, generated .formatter.exs (#71)

* rebuild DSL inner workings for extensibility (#70)

* add `after_compile` and validate primary key

* remove name/type from ash core

* use option schemas in the interface (#30)

* add data layer custom filters

* various fixes, upsert, relationship changes

* add side_load

* add `timestamps/0` to attributes

* support writeable?: false attributes

### Bug Fixes:

* simplify dsl building using `on_load`

* use proper errors everywhere

* changeset + set_state issues

* small fixes

* in predicate + engine errors

* remove benchee, ensure mnesia always uses transactions

* try clearing cache to fix CI

* stop gitignoring the mnesia data layer

* try to fix ash.formatter task

* test/improve parallelizable requests

* require that resources have primary keys

* move to simpler transaction logic

* fix tests/credo

* fix tests, add tests for gt/lt filters

* set persistent_term properly

* use authorization filters in side loads

* remove reverse relationships

* many filter/side load fixes/improvements

* allow side_load option on create/update

* raised error message contents

* parent error messages

* relationship path clause

* consider nested entities in ash.formatter

* compile application in ash.formatter task

* dialyzer warnings

* honor the `authorize?` flag

* account for action/actor args to interface

* remove the rest of the deps on name/type

* add `resource_module?/1` back to `Ash`

* references to error handling code

* fix empty filter checks

* typo in workflow

* set HEX_API_KEY environment from secret

* typo in version name

* remove broken CI key

* typo in function name

* remove test warning from git_ops

* combine filters properly

* dependencies

* honor returned check filters

* side_load queries

* many to many relationship side loads

* protect against double data resolution

* various small improvements

* various bugs

* `verbose?` now prints auth log

* read generated fields after writes

* more fixes

* fetch_attr right

* attribute defaults

* use attribute name in error

* fetch_attr return value

* writeable is not a word

* correct writeable attribute logic

* required attribute validations

* use resource's primary key config

* fix resolvable requests logic

### Improvements:

* add `date` support (#68)

* huge query/filter overhaul

* remove pagination, add query struct (unused)

* filters: simplify/improve inspect logic

* add DslBuilder pattern for deriving dsl blocks

* add new types
