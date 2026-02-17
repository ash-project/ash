# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [1.3.0](https://github.com/elixir-telemetry/telemetry/tree/v1.3.0)

### Added

- Ability to return extra measurements from `telemetry:span/3`.

### Changed

- Rewrite docs from edoc to OTP 27 `-moduledoc`/`-doc`.

  Internal macros `?DOC` and `?MODULEDOC` are used. They are no-ops prior to OTP 27.

## [1.2.1](https://github.com/elixir-telemetry/telemetry/tree/v1.2.0)

### Fixed

- Fixed a local handler warning log when using `telemetry_test`. (#124)

## [1.2.0](https://github.com/elixir-telemetry/telemetry/tree/v1.2.0)

### Added

- Added `telemetry_test` module for testing telemetry events. (#118)

## [1.1.0](https://github.com/elixir-telemetry/telemetry/tree/v1.1.0)

### Added

- Added `monotonic_time` measurement to all span events. (#92)
- Added a `[telemetry, handler, failure]` event emitted when any handler fails and is detached. (#98)
- Added a `mix.exs` file, lowering the memory footprint of compilation in Elixir projects. (#103)

## [1.0.0](https://github.com/elixir-telemetry/telemetry/tree/v1.0.0)

There are no changes in the 1.0.0 release - it marks the stability of the API.

## [0.4.3](https://github.com/elixir-telemetry/telemetry/tree/v0.4.3)

This release improves the `telemetry:span/3` function by adding the `telemetry_span_context` metadata
to all span events. The new metadata enables correlating span events that belong to the same span.

### Added

- Added `telemetry_span_context` metadata to all events emitted by `telemetry:span/3`.

## [0.4.2](https://github.com/elixir-telemetry/telemetry/tree/v0.4.2)

### Added

- Added the `telemetry:span/3` function.

## [0.4.1](https://github.com/elixir-telemetry/telemetry/tree/v0.4.1)

Apart from the code changes listed below, this release includes a few improvements
to the documentation.

### Changed

- Calls to `execute/3` when `telemetry` application is not started no longer cause an error.

## [0.4.0](https://github.com/elixir-telemetry/telemetry/tree/v0.4.0)

A single event value has been replaced by a map of measurements. Now it is up to the consumer of the
event to decide what part of the payload is important. This is useful in cases where event indicates
that a thing happened but there are many properties describing it. For example, a database query
event may include total time, decode time, wait time and other measurements.

### Changed

- `execute/3` now accepts a map of measurements instead of event value

### Deprecated

- `:telemetry.execute/3` with an event value in favour of `:telemetry.execute/3` with a map of
  measurements. If the event value is provided, it is put in a map under a `:value` key and provided
  as measurements to a handler function.

## [0.3.0](https://github.com/elixir-telemetry/telemetry/tree/v0.3.0)

This release marks the conversion from Elixir to Erlang. This is a breaking change, but the benefits
largely surpass the drawbacks - Telemetry core can now be used by all projects running on the BEAM,
regardless of the language they're written in.

### Added

- Added `:telemetry.handler/0`, `:telemetry.handler_function/0` and `:telemetry.handler_config/0`
  types.

### Changed

- The library has been rewritten to Erlang. In Elixir, `:telemetry` module has to be used in place
  of `Telemetry`. In Erlang, `telemetry` module has to be used in place of `'Elixir.Telemetry'`;
- `:telemetry.list_handlers/1` returns a list of maps (of type `:telemetry.handler/0`) instead of
  a list of tuples;
- `:telemetry.attach/4` and `:telemetry.attach_many/4` replaced the 5-arity versions and now accept
  an anonymous function for the handler function instead of a module and function name.

### Removed

- Removed `:telemetry.attach/5` and `:telemetry.attach_many/5` - 4-arity versions need to be used
  now instead.

## [0.2.0](https://github.com/elixir-telemetry/telemetry/tree/v0.2.0)

The main point of this release is to mark base Telemetry API as stable, so that other libraries can
rely on it without worrying about backwards compatibility.

### Removed

- Removed `Telemetry.attach/4` and `Telemetry.attach_many/4` - the handler config is now required.

### Fixed

- Fixed type specs which were producing Dialyzer errors.

## [0.1.0](https://github.com/elixir-telemetry/telemetry/tree/v0.1.0)

First release of Telemetry library.
