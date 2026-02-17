<!--
SPDX-FileCopyrightText: 2020 Zach Daniel
SPDX-FileCopyrightText: 2024 splode contributors <https://github.com/ash-project/splode/graphs.contributors>

SPDX-License-Identifier: MIT
-->

# Change Log

All notable changes to this project will be documented in this file.
See [Conventional Commits](Https://conventionalcommits.org) for commit guidelines.

<!-- changelog -->

## [v0.3.0](https://github.com/ash-project/splode/compare/v0.2.10...v0.3.0) (2026-01-16)




### Features:

* support filtering stacktrace modules in Splode errors by Zach Daniel

## [v0.2.10](https://github.com/ash-project/splode/compare/v0.2.9...v0.2.10) (2026-01-04)




### Improvements:

* add `error_class?` flag to Splode.ErrorClass structs by Zach Daniel

## [v0.2.9](https://github.com/ash-project/splode/compare/v0.2.8...v0.2.9) (2025-02-28)




### Improvements:

* show docs for exception function & keys in doc string

## [v0.2.8](https://github.com/ash-project/splode/compare/v0.2.7...v0.2.8) (2025-01-27)




### Improvements:

* accept raw stacktraces in exception

## [v0.2.7](https://github.com/ash-project/splode/compare/v0.2.6...v0.2.7) (2024-10-30)




### Bug Fixes:

* remove IO.inspect

## [v0.2.6](https://github.com/ash-project/splode/compare/v0.2.5...v0.2.6) (2024-10-30)




### Bug Fixes:

* don't miss adding bread crumbs in specific cases

### Improvements:

* better formatting of bread crumbs

## [v0.2.5](https://github.com/ash-project/splode/compare/v0.2.4...v0.2.5) (2024-10-29)




### Improvements:

* support `.unwrap!/2` on generated splode modules

* Don't show `Process.info` in stacktraces. (#10)

## [v0.2.4](https://github.com/ash-project/splode/compare/v0.2.3...v0.2.4) (2024-05-02)




### Bug Fixes:

* properly return unwrapped list of errors when `:special` class is used

* Splode.Error.message/1 (#7)

## [v0.2.3](https://github.com/ash-project/splode/compare/v0.2.2...v0.2.3) (2024-04-23)




### Improvements:

* make `set_path/2` overridable

## [v0.2.2](https://github.com/ash-project/splode/compare/v0.2.1...v0.2.2) (2024-04-05)




### Bug Fixes:

* loosen elixir language version requirement

## [v0.2.1](https://github.com/ash-project/splode/compare/v0.2.0...v0.2.1) (2024-03-28)




### Bug Fixes:

* properly produce unknown error on empty list provided

## [v0.2.0](https://github.com/ash-project/splode/compare/v0.1.1...v0.2.0) (2024-03-18)
### Breaking Changes:

* message/1 instead of splode_message/1



### Improvements:

* add `use Splode.ErrorClass`

* store the module that created an error in the `splode` key

## [v0.1.1](https://github.com/ash-project/splode/compare/v0.1.0...v0.1.1) (2024-03-15)

- Creating a new version so I can change the hex package description 😂

## [v0.1.0](https://github.com/ash-project/splode/compare/v0.1.0...v0.1.0) (2024-03-14)
