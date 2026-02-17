# Changelog

## v1.2.0

  * Add `StreamData.shuffle/1`.

## v1.1.3

  * Fix compilation warnings with current Elixir version (1.18) and upcoming Elixir version.

## v1.1.2

  * Fix one more Elixir 1.17 warning.

## v1.1.1

  * Add warnings-free support for Elixir 1.17.

## v1.1.0

  * Drop support for Elixir 1.11 and lower, require Elixir 1.12+ now.

## v1.0.0

No changes. This is just the 1.0 release. Happy fuzzying!

## v0.6.0

### Bug Fixes

  * Consider max chars when generating atoms.
  * Fix some small issues in `StreamData.nonempty_improper_list_of/2`.

### Features

  * Add `StreamData.non_negative_integer/0`.
  * Add `StreamData.repeatedly/1`.
  * Add `StreamData.chardata/0`.
  * Add `StreamData.codepoint/1`.
  * Add support for *not implemented* properties (which are just `property "some name"`, without a `do`/`/end` body). This is on par with ExUnit's `test/1`.
  * Add support for stepped ranges in `StreamData.integer/1`.
  * Add support for required keys in `StreamData.optional_map/2`.
  * Add `:utf8` option in `StreamData.string/1`.

## v0.5.0

  * Slightly improve the shrinking algorigthm.
  * Add `StreamData.map_of/2`.
  * Fix a bug around the `:max_shrinking_steps` option.
  * Fix a runtime warning with Elixir 1.10.

## v0.4.3

  * Improve the frequency of terms in `StreamData.term/0`
  * Fix a bug in `StreamData.positive_integer/0` that would crash with a generation size of `0`.
  * Support inline `, do:` in `gen all` and `check all`.
  * Support `:initial_seed` in `check all`.
  * Export formatter configuration for `check all` and `gen all`.
  * Add `StreamData.seeded/2`.

## v0.4.2

  * Fix a bug when shrinking boolean values generated with `StreamData.boolean/0`

## v0.4.1

  * Import all functions/macros from `ExUnitProperties` when `use`d
  * Various optimizations
  * Add the `:max_run_time` configuration option to go together with `:max_runs`
  * Add support for `:do` syntax in `gen all`/`check all`

## v0.4.0

  * Add a `StreamData.term/0` generator
  * Bump the number of allowed consecutive failures in `StreamData.filter/3` and `StreamData.bind_filter/3`
  * Improve error message for `StreamData.filter/3`
  * Add `ExUnitProperties.pick/1`
  * Add `Enumerable.slice/1` to `StreamData` structs
  * Improve the performance of `StreamData.bitstring/1`

#### Breaking changes

  * Remove `StreamData.unquoted_atom/0` in favour of `StreamData.atom(:unquoted | :alias)`
  * Start behaving like filtering when patterns don't match in `check all` or `gen all`
  * Remove special casing of `=` clauses in `check all` and `gen all`
  * Introduce `StreamData.float/1` replacing `StreamData.uniform_float/0`

## v0.3.0

  * Add length-related options to `StreamData.string/2`
  * Introduce `StreamData.positive_integer/0`
  * Raise a better error message on invalid generators
  * Fix the `StreamData.t/0` type
  * Add support for `rescue/catch/after` in `ExUnitProperties.property/2,3`
  * Introduce `StreamData.optional_map/1`
  * Add support for keyword lists as argument to `StreamData.fixed_map/1`

#### Breaking changes

  * Change the arguments to `StreamData.string/2` so that it can take `:ascii`, `:alphanumeric`, `:printable`, a range, or a list of ranges or single codepoints
  * Rename `PropertyTest` to `ExUnitProperties` and introduce `use ExUnitProperties` to use in tests that use property-based testing

## v0.2.0

  * Add length-related options to `StreamData.list_of/2`, `StreamData.uniq_list_of/1`, `StreamData.binary/1`
  * Add a `StreamData.bitstring/1` generator

#### Breaking changes

  * Remove `StreamData.string_from_chars/1`, `StreamData.ascii_string/0`, and `StreamData.alphanumeric_string/0` in favour of `StreamData.string/1`
  * Rename `StreamData.non_empty/1` to `StreamData.nonempty/1`
  * Rename `StreamData.int/0,1` to `StreamData.integer/0,1`
  * Rename `StreamData.no_shrink/` to `StreamData.unshrinkable/1`
  * Remove `StreamData.uniq_list_of/3` in favour of `StreamData.uniq_list_of/2` (which takes options)

## v0.1.1

  * Fix a bug with `check all` syntax where it wouldn't work with assignments in the clauses.
