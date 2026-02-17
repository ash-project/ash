# ETS

`:ets`, the Elixir way

[![Build status](https://github.com/TheFirstAvenger/ets/actions/workflows/ci.yml/badge.svg)](https://github.com/TheFirstAvenger/ets/actions/workflows/ci.yml)
[![Coverage Status](https://coveralls.io/repos/github/TheFirstAvenger/ets/badge.svg?branch=master)](https://coveralls.io/github/TheFirstAvenger/ets?branch=master)
[![Project license](https://img.shields.io/hexpm/l/ets.svg)](https://unlicense.org/)
[![Hex.pm package](https://img.shields.io/hexpm/v/ets.svg)](https://hex.pm/packages/ets)
[![Hex.pm downloads](https://img.shields.io/hexpm/dt/ets.svg)](https://hex.pm/packages/ets)

ETS is a set of Elixir modules that wrap Erlang Term Storage (`:ets`).

## Current Features

-   `ETS.Set` - wraps `:set` and `:ordered_set`
-   `ETS.Bag` - wraps `:bag` and `:duplicate_bag`
-   `ETS.KeyValueSet` - extension of `ETS.Set` that abstracts away tuple and key index concepts into simple key/value inputs/outputs.
-   Most used functions from `:ets` replicated for all wrappers
-   Returns {:error, reason} tuples (or raises in ! versions) for:
    -   `:table_not_found`
    -   `:table_already_exists`
    -   `:key_not_found`
    -   `:invalid_record` (when inserting non-tuples)
    -   `:record_too_small` (tuple smaller than keypos index)
    -   `:position_out_of_bounds` (`lookup` with a pos > length of one of the results)
    -   `:invalid_select_spec`
    -   `:write_protected` - trying to write to a protected or private table from a different process than the owner
    -   `:read_protected` - trying to read from a private table from a different process than the owner

## Design Goals

The purpose of this package is to improve the developer experience when both learning and interacting with Erlang Term Storage.

This will be accomplished by:

-   Conforming to Elixir standards:
    -   Two versions of all functions:
        -   Main function (e.g. `get`) returns `{:ok, return}`/`{:error, reason}` tuples.
        -   Bang function (e.g. `get!`) returns unwrapped value or raises on :error.
    -   All options specified via keyword list.
-   Wrapping unhelpful `ArgumentError`'s with appropriate error returns.
    -   Avoid adding performance overhead by using try/rescue instead of pre-validation
    -   On rescue, try to determine what went wrong (e.g. missing table) and return appropriate error
    -   Fall back to `{:error, :unknown_error}` (logging details) if unable to determine reason.
-   Appropriate error returns/raises when encountering `$end_of_table`.
-   Providing Elixir friendly documentation.
-   Providing `ETS.Set` and `ETS.Bag` modules with appropriate function signatures and error handling.
    -   `ETS.Set.get` returns a single item (or nil/provided default) instead of list as sets never have multiple records for a key.
-   Providing abstractions on top of the two base modules for specific usages
    -   `ETS.KeyValueSet` abstracts away the concept of tuple records, replacing it with standard key/value interactions.

## Changes

For a list of changes, see the [changelog](CHANGELOG.md)

## Usage

### Creating ETS Tables

ETS Tables can be created using the `new` function of the appropriate module, either `ETS.Set`
(for ordered and unordered sets) or `ETS.Bag` (for duplicate or non-duplicate bags).
See module documentation for more examples and documentation, including a guide on [What type of ETS table should I use?](lib/ets.ex).

#### Create Examples

```elixir
iex> {:ok, set} = Set.new(ordered: true, keypos: 3, read_concurrency: true, compressed: false)
iex> Set.info!(set)[:read_concurrency]
true

# Named :ets tables via the name keyword
iex> {:ok, set} = Set.new(name: :my_ets_table)
iex> Set.info!(set)[:name]
:my_ets_table
iex> {:ok, set} = Set.wrap_existing(:my_ets_table)
iex> set = Set.wrap_existing!(:my_ets_table)
```

### Adding/Updating/Retrieving records in Sets

To add records to an ETS table, use `put` or `put_new` with a tuple record or a list of tuple records.
`put` will overwrite existing records with the same key. `put_new` not insert if the key
already exists. When passing a list of tuple records, all records are inserted in an atomic and
isolated manner, but with `put_new` no records are inserted if at least one existing key is found.

#### Set Examples

```elixir
iex> set = Set.new!(ordered: true)
iex> |> Set.put!({:a, :b})
iex> |> Set.put!({:a, :c}) # Overwrites entry from previous line
iex> |> Set.put!({:c, :d})
iex> Set.get(:a)
{:ok, {:a, :c}}
iex> Set.to_list(set)
{:ok, [{:a, :c}, {:c, :d}]}

iex> Set.new!(ordered: true)
iex> |> Set.put!({:a, :b})
iex> |> Set.put_new!({:a, :c}) # Doesn't insert due to key :a already existing
iex> |> Set.to_list!()
[{:a, :b}]
```

#### Bag Examples

```elixir
iex> bag = Bag.new!()
iex> |> Bag.add!({:a, :b})
iex> |> Bag.add!({:a, :c})
iex> |> Bag.add!({:a, :c}) # Adds dude to duplicate: true
iex> |> Bag.add!({:c, :d})
iex> Bag.lookup(set, :a)
{:ok, [{:a, :b}, {:a, :c}, {:a, :c}]}
iex> Bag.to_list(bag)
{:ok, [{:a, :b}, {:a, :c}, {:a, :c}, {:c, :d}]}
iex> Bag.add_new!(bag, {:a, :z}) # Doesn't add due to key :a already existing
iex> Bag.to_list(bag)
{:ok, [{:a, :b}, {:a, :c}, {:a, :c}, {:c, :d}]}

iex> bag = Bag.new!(duplicate: false)
iex> |> Bag.add!({:a, :b})
iex> |> Bag.add!({:a, :c})
iex> |> Bag.add!({:a, :c}) # Doesn't add dude to duplicate: false
iex> |> Bag.add!({:c, :d})
iex> Bag.lookup(bag, :a)
{:ok, [{:a, :b}, {:a, :c}]}
iex> Bag.to_list(bag)
{:ok, [{:a, :b}, {:a, :c}, {:c, :d}]}
```

## Current Progress

### Base Modules

-   [x] `ETS`
    -   [x] All
-   [x] `ETS.Set`
    -   [x] Put (insert)
    -   [x] Get (lookup)
    -   [x] Get Element
    -   [x] Delete
    -   [x] Delete All
    -   [x] First
    -   [x] Next
    -   [x] Last
    -   [x] Previous
    -   [x] Match
    -   [x] Select
    -   [x] Select Delete
    -   [x] Has Key (Member)
    -   [x] Info
    -   [x] Delete
    -   [x] To List (tab2list)
    -   [x] Wrap
-   [x] `ETS.Bag`
    -   [x] Add (insert)
    -   [x] Lookup
    -   [x] Lookup Element
    -   [x] Delete
    -   [x] Delete All
    -   [x] Match
    -   [x] Select
    -   [x] Select Delete
    -   [x] Has Key (Member)
    -   [x] Info
    -   [x] Delete
    -   [x] To List (tab2list)
    -   [x] Wrap

### Abstractions

-   [x] `ETS.KeyValueSet`
    -   [x] New
    -   [x] Wrap Existing
    -   [x] Put
    -   [x] Put New
    -   [x] Get
    -   [x] Info
    -   [x] Get Table
    -   [x] First
    -   [x] Last
    -   [x] Next
    -   [x] Previous
    -   [x] Has Key
    -   [x] Delete
    -   [x] Delete Key
    -   [x] Delete All
    -   [x] To List

## Installation

`ETS` can be installed by adding `ets` to your list of dependencies in `mix.exs`:

```elixir
def deps do
  [
    {:ets, "~> 0.9.0"}
  ]
end
```

Docs can be found at <https://hexdocs.pm/ets>.

## Contributing

Contributions welcome. Specifically looking to:

-   Add remainder of functions ([See Erlang Docs](http://erlang.org/doc/man/ets.html])).
-   Discover and add zero-impact recovery for any additional possible `:ets` `ArgumentError`s.
