# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/).

## Unreleased

<!-- Add your changelog entry to the relevant subsection -->

<!-- ### Added | Changed | Deprecated | Removed | Fixed | Security -->

## [5.1.4] - 2025-07-14

<!--------------------- Don't add new entries after this line --------------------->

### Changed

- Changed how Date, Time, NaiveDateTime, and DateTime are encode when nested in a map, fixes [#207](https://github.com/ufirstgroup/ymlr/issues/207)

### Fixed

- fixed typo in spec: `idnent_level` => `indent_level`

## [5.1.3] - 2024-03-12

### Fixed

- Quote all versions of booleans when given as strings [#188](https://github.com/ufirstgroup/ymlr/issues/188), [#190](https://github.com/ufirstgroup/ymlr/issues/190)

## [5.1.2] - 2024-02-06

### Fixed

- quote strings strings starting with % [#184](https://github.com/ufirstgroup/ymlr/pull/184)

## [5.1.1] - 2024-01-12

### Fixed

- handle quoted strings containing single quotes (should always use double quotes) [#179](https://github.com/ufirstgroup/ymlr/issues/179), [#181](https://github.com/ufirstgroup/ymlr/pull/181)
- handle string with leading or trailing whitespaces [#180](https://github.com/ufirstgroup/ymlr/issues/180), [#181](https://github.com/ufirstgroup/ymlr/pull/181)

## [5.1.0] - 2023-12-27

### Added

- Support for escape and unicode characters [#98](https://github.com/ufirstgroup/ymlr/pull/98)
- Refactor `Ymlr.Encode` to make it faster [#171](https://github.com/ufirstgroup/ymlr/pull/171).
- Handle special floats `.Inf` and `.Nan`. [#170](https://github.com/ufirstgroup/ymlr/issues/170), [#172](https://github.com/ufirstgroup/ymlr/issues/172)
- Script for benchmarking

### Fixed

- Put single quotes around indicator chars when it is the entire value. [#177](https://github.com/ufirstgroup/ymlr/issues/177), [#178](https://github.com/ufirstgroup/ymlr/pull/178)

## [5.0.0] - 2023-11-03

Although this release contains only fixes but as the encoding changes, we
consider it a breaking change. Hence the major bump.

### Fixed

- handle "\n" strings - [#157](https://github.com/ufirstgroup/ymlr/issues/157),[#159](https://github.com/ufirstgroup/ymlr/pull/159)
- handle empty list - [#163](https://github.com/ufirstgroup/ymlr/pull/163)

## [4.2.0] - 2023-08-18

### Added

- Adds an option `sort_maps` to encode map with entries sorted - [#139](https://github.com/ufirstgroup/ymlr/issues/139),[#144](https://github.com/ufirstgroup/ymlr/pull/144)
- Quote values beginning with `:{` - [#140](https://github.com/ufirstgroup/ymlr/issues/140),[#141](https://github.com/ufirstgroup/ymlr/pull/141)

## [4.1.0] - 2023-04-23

### Added

- `Ymlr.Encoder` - Option `except: :defaults` to exclude default values

## [4.0.0] - 2023-04-16

### Breaking Change

**Up to and including version 3.x, `ymlr` would encode structs out of the box.
This version introduces protocols so and structs need to `@derive Ymlr.Encoder`
in order to be encoded. See the documentation of the `Ymlr.Encoder` protocol
for further information.**

### Changed

- A protocol based implementation was added which give more freedom to users of this library - [#118](https://github.com/ufirstgroup/ymlr/pull/118)

### Added

- Adds an option `atoms` to encode atom map keys with a leading colon.
- Tuples are now encoded as lists.

## [3.0.1] - 2022-09-05

### Fixed

- Wrap map keys in doouble quotes when required ([#94](https://github.com/ufirstgroup/ymlr/issues/94), [#95](https://github.com/ufirstgroup/ymlr/pull/95))
- Encode structs by turning them to lists before mapping over them

## [3.0.0] - 2022-08-07

**In this release we changed the way `DateTime` is encoded (see below). This can be a breaking change if you rely on the old date format with spaces. Because of this change, version 3.0.0 is now again compatible with Elixir 1.10**

### Changed

- use `Enum.map_join/3` indead of `Enum.map/2` and `Enum.join/2` as it's more efficient according to credo recommendations
- Change the serialization of timestamps to use the canonical (iso8601) format, i.e. before: `2022-07-31 14:48:48.000000000 Z` and now: `"2022-07-31T14:48:48Z"` ([#87](https://github.com/ufirstgroup/ymlr/issues/87), [#90](https://github.com/ufirstgroup/ymlr/pull/90))

## [2.0.0] - 2021-04-02

### Removed

- 2.0 and upwards don't support Elixir 1.10 anymore. Use version 1.x for Elixir 1.10 support.

### Added

- Date and DateTime support (#17)

### Chores

- yaml_elixir upgraded to 2.6.0
- excoveralls upgraded to 0.14.0
- ex_doc upgraded to 0.24.1
- credo upgraded to 1.5.5
- dialyxir upgraded to 1.1.0

## [1.1.0] - 2021-04-02

### Added

- Date and DateTime support (#17)

### Chores

- yaml_elixir upgraded to 2.6.0
- excoveralls upgraded to 0.14.0
- ex_doc upgraded to 0.24.1
- credo upgraded to 1.5.5
- dialyxir upgraded to 1.1.0

## [1.0.1] - 2020-09-22

### Changed

- Rescue ArgumentError exception for oversize floats according to Float.parse/1 doc

## [1.0.0] - 2020-08-21

No changes in this release. We have tested the library on a big bunch of CRDs and feel confident to publish a sable relese.

## [0.0.1] - 2020-07-31

First ymlr beta release
