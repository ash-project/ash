# Decimal

Arbitrary precision decimal arithmetic.

## Concept

Decimal represents values internally using three integers: a sign, a coefficient, and an exponent.
In this way, numbers of any size and with any number of decimal places can be represented exactly.

```elixir
Decimal.new(_sign = 1, _coefficient = 42, _exponent = 0) #=> Decimal.new("42")
Decimal.new(-1, 42, 0) #=> Decimal.new("-42")
Decimal.new(1, 42, -1) #=> Decimal.new("4.2")
Decimal.new(1, 42, -20) #=> Decimal.new("4.2E-19")
Decimal.new(1, 42, 20) #=> Decimal.new("4.2E+21")
Decimal.new(1, 123456789987654321, -9) #=> Decimal.new("123456789.987654321")
```

For calculations, the amount of desired precision - that is, the number of
decimal digits in the coefficient - can be specified.

## Usage

Add Decimal as a dependency in your `mix.exs` file:

```elixir
def deps do
  [{:decimal, "~> 2.0"}]
end
```

Next, run `mix deps.get` in your shell to fetch and compile `Decimal`. Start an
interactive Elixir shell with `iex -S mix`:

```elixir
iex> alias Decimal, as: D
iex> D.add(6, 7)
Decimal.new("13")
iex> D.div(1, 3)
Decimal.new("0.3333333333333333333333333333")
iex> D.new("0.33")
Decimal.new("0.33")
```

## Examples

### Using the context

The context specifies the maximum precision of the result of calculations and
the rounding algorithm if the result has a higher precision than the specified
maximum. It also holds the list of trap enablers and the current set
flags.

The context is stored in the process dictionary. You don't have to pass the
context around explicitly and the flags will be updated automatically.

The context is accessed with `Decimal.Context.get/0` and set with
`Decimal.Context.set/1`. It can be set temporarily with
`Decimal.Context.with/2`.

```elixir
iex> D.Context.get()
%Decimal.Context{flags: [:rounded, :inexact], precision: 9, rounding: :half_up,
 traps: [:invalid_operation, :division_by_zero]}

iex> D.Context.with(%D.Context{precision: 2}, fn -> IO.inspect D.Context.get() end)
%Decimal.Context{flags: [], precision: 2, rounding: :half_up,
 traps: [:invalid_operation, :division_by_zero]}
%Decimal.Context{flags: [], precision: 2, rounding: :half_up,
 traps: [:invalid_operation, :division_by_zero]}

iex> D.Context.set(%D.Context{D.Context.get() | traps: []})
:ok

iex> D.Context.get()
%Decimal.Context{flags: [:rounded, :inexact], precision: 9, rounding: :half_up,
 traps: []}
```

### Precision and rounding

Use `:precision` option to limit the amount of decimal digits in the
coefficient of any calculation result:

```elixir
iex> D.Context.set(%D.Context{D.Context.get() | precision: 9})
:ok

iex> D.div(100, 3)
Decimal.new("33.3333333")

iex> D.Context.set(%D.Context{D.Context.get() | precision: 2})
:ok

iex> D.div(100, 3)
Decimal.new("33")
```

The `:rounding` option specifies the algorithm and precision of the rounding
operation:

```elixir
iex> D.Context.set(%D.Context{D.Context.get() | rounding: :half_up})
:ok

iex> D.div(31, 2)
Decimal.new("16")

iex> D.Context.set(%D.Context{D.Context.get() | rounding: :floor})
:ok

iex> D.div(31, 2)
Decimal.new("15")
```

### Comparisons

Using comparison operators (`<`, `=`, `>`) with two or more decimal digits may
not produce accurate result. Instead, use comparison functions.

```elixir
iex> D.compare(-1, 0)
:lt
iex> D.compare(0, -1)
:gt
iex> D.compare(0, 0)
:eq

iex> D.equal?(-1, 0)
false
iex> D.equal?(0, "0.0")
true
```

### Flags and trap enablers

When an exceptional condition is signalled, its flag is set in the current
context. `Decimal.Error` will be raised if the trap enabler is set.

```elixir
iex> D.Context.set(%D.Context{D.Context.get() | rounding: :floor, precision: 2})
:ok

iex> D.Context.get().traps
[:invalid_operation, :division_by_zero]

iex> D.Context.get().flags
[]

iex> D.div(31, 2)
Decimal.new("15")

iex> D.Context.get().flags
[:inexact, :rounded]
```

`:inexact` and `:rounded` flag were signalled above because the result of the
operation was inexact given the context's precision and had to be rounded to
fit the precision. `Decimal.Error` was not raised because the signals' trap
enablers weren't set.

```elixir
iex> D.Context.set(%D.Context{D.Context.get() | traps: D.Context.get().traps ++ [:inexact]})
:ok

iex> D.div(31, 2)
** (Decimal.Error)
```

The default trap enablers, such as `:division_by_zero`, can be unset:

```elixir
iex> D.Context.get().traps
[:invalid_operation, :division_by_zero]

iex> D.div(42, 0)
** (Decimal.Error)

iex> D.Context.set(%D.Context{D.Context.get() | traps: [], flags: []})
:ok

iex> D.div(42, 0)
Decimal.new("Infinity")

iex> D.Context.get().flags
[:division_by_zero]
```

### Mitigating rounding errors

TODO

## License

   Copyright 2013 Eric Meadows-Jönsson

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.
