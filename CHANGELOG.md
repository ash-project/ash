# Change Log

All notable changes to this project will be documented in this file.
See [Conventional Commits](Https://conventionalcommits.org) for commit guidelines.

<!-- changelog -->

## [v0.1.1](https://github.com/ash-project/ash/compare/0.1.1...v0.1.1) (2020-06-01)




### Features:

* add data layer custom filters

* various fixes, upsert, relationship changes

* add side_load

* add `timestamps/0` to attributes

* support writeable?: false attributes

### Bug Fixes:

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

* huge query/filter overhaul

* remove pagination, add query struct (unused)

* filters: simplify/improve inspect logic

* add DslBuilder pattern for deriving dsl blocks

* add new types
