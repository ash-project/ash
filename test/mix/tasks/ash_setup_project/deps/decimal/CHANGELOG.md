# CHANGELOG

## v2.3.0 (2024-12-13)

* Implement the upcoming [`JSON.Encoder`](https://hexdocs.pm/elixir/main/JSON.Encoder.html)
  protocol

## v2.2.0 (2024-11-13)

* Add `Decimal.gte?/2` and `Decimal.lte?/2`
* Add `Decimal.compare/3` and `Decimal.eq?/3` with threshold as parameter

## v2.1.1 (2023-04-26)

Decimal v2.1 requires Elixir v1.8+.

### Bug fixes

* Fix `Decimal.compare/2` when comparing against `0`

## v2.1.0 (2023-04-26)

Decimal v2.1 requires Elixir v1.8+.

### Enhancements

* Improve error message from `Decimal.to_integer/1` during precision loss
* `Inspect` protocol implementation returns strings in the `Decimal.new(...)` format
* Add `Decimal.scale/1`
* Optimize `Decimal.compare/2` for numbers with large exponents

### Bug fixes

* Fix `Decimal.integer?/1` spec
* Fix `Decimal.integer?/1` check on 0 with >1 significant digits

## v2.0.0 (2020-09-08)

Decimal v2.0 requires Elixir v1.2+.

### Enhancements

* Add `Decimal.integer?/1`

### Breaking changes

* Change `Decimal.compare/2` to return `:lt | :eq | :gt`
* Change `Decimal.cast/1` to return `{:ok, t} | :error`
* Change `Decimal.parse/1` to return `{t, binary} | :error`
* Remove `:message` and `:result` fields from `Decimal.Error`
* Remove sNaN
* Rename qNaN to NaN
* Remove deprecated support for floats in `Decimal.new/1`
* Remove deprecated `Decimal.minus/1`
* Remove deprecated `Decimal.plus/1`
* Remove deprecated `Decimal.reduce/1`
* Remove deprecated `Decimal.with_context/2`, `Decimal.get_context/1`, `Decimal.set_context/1`,
  and `Decimal.update_context/1`
* Remove deprecated `Decimal.decimal?/1`

### Deprecations

* Deprecate `Decimal.cmp/2`

## v1.9.0 (2020-09-08)

### Enhancements

* Add `Decimal.negate/1`
* Add `Decimal.apply_context/1`
* Add `Decimal.normalize/1`
* Add `Decimal.Context.with/2`, `Decimal.Context.get/1`, `Decimal.Context.set/2`,
  and `Decimal.Context.update/1`
* Add `Decimal.is_decimal/1`

### Deprecations

* Deprecate `Decimal.minus/1` in favour of the new `Decimal.negate/1`
* Deprecate `Decimal.plus/1` in favour of the new `Decimal.apply_context/1`
* Deprecate `Decimal.reduce/1` in favour of the new `Decimal.normalize/1`
* Deprecate `Decimal.with_context/2`, `Decimal.get_context/1`, `Decimal.set_context/2`,
  and `Decimal.update_context/1` in favour of new functions on the `Decimal.Context` module
* Deprecate `Decimal.decimal?/1` in favour of the new `Decimal.is_decimal/1`

## v1.8.1 (2019-12-20)

### Bug fixes

* Fix Decimal.compare/2 with string arguments
* Set :signal on error

## v1.8.0 (2019-06-24)

### Enhancements

* Add `Decimal.cast/1`
* Add `Decimal.eq?/2`, `Decimal.gt?/2`, and `Decimal.lt?/2`
* Add guards to `Decimal.new/3` to prevent invalid Decimal numbers

## v1.7.0 (2019-02-16)

### Enhancements

* Add `Decimal.sqrt/1`

## v1.6.0 (2018-11-22)

### Enhancements

* Support for canonical XSD representation on `Decimal.to_string/2`

### Bugfixes

* Fix exponent off-by-one when converting from decimal to float
* Fix negative?/1 and positive?/1 specs

### Deprecations

* Deprecate passing float to `Decimal.new/1` in favor of `Decimal.from_float/1`

## v1.5.0 (2018-03-24)

### Enhancements

* Add `Decimal.positive?/1` and `Decimal.negative?/1`
* Accept integers and strings in arithmetic functions, e.g.: `Decimal.add(1, "2.0")`
* Add `Decimal.from_float/1`

### Soft deprecations (no warnings emitted)

* Soft deprecate passing float to `new/1` in favor of `from_float/1`

## v1.4.1 (2017-10-12)

### Bugfixes

* Include the given value as part of the error reason
* Fix `:half_even` `:lists.last` bug (empty signif)
* Fix error message for round
* Fix `:half_down` rounding error when remainder is greater than 5
* Fix `Decimal.new/1` float conversion with bigger precision than 4
* Fix precision default value

## v1.4.0 (2017-06-25)

### Bugfixes

* Fix `Decimal.to_integer/1` for large coefficients
* Fix rounding of ~0 values
* Fix errors when comparing and adding two infinities
