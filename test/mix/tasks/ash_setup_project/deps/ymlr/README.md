# ymlr - A YAML Encoder for Elixir

ymlr - A YAML encoder for Elixir.

[![Module Version](https://img.shields.io/hexpm/v/ymlr.svg)](https://hex.pm/packages/ymlr)
[![Coverage Status](https://coveralls.io/repos/github/ufirstgroup/ymlr/badge.svg?branch=main)](https://coveralls.io/github/ufirstgroup/ymlr?branch=main)
[![Last Updated](https://img.shields.io/github/last-commit/ufirstgroup/ymlr.svg)](https://github.com/ufirstgroup/ymlr/commits/main)

[![Build Status Code Qualits](https://github.com/ufirstgroup/ymlr/actions/workflows/code_quality.yaml/badge.svg)](https://github.com/ufirstgroup/ymlr/actions/workflows/code_quality.yaml)
[![Build Status Elixir](https://github.com/ufirstgroup/ymlr/actions/workflows/elixir_matrix.yaml/badge.svg)](https://github.com/ufirstgroup/ymlr/actions/workflows/elixir_matrix.yaml)

[![Hex Docs](https://img.shields.io/badge/hex-docs-lightgreen.svg)](https://hexdocs.pm/ymlr/)
[![Total Download](https://img.shields.io/hexpm/dt/ymlr.svg)](https://hex.pm/packages/ymlr)
[![License](https://img.shields.io/hexpm/l/ymlr.svg)](https://github.com/ufirstgroup/ymlr/blob/main/LICENSE)

## Installation

The package can be installed by adding `ymlr` to your list of dependencies in `mix.exs`:

```elixir
def deps do
  [
    {:ymlr, "~> 5.0"}
  ]
end
```

## Examples

See The usage livebook `usage.livemd` for more detailed examples.

### Encode a single document - optionally with comments:

```elixir
  iex> Ymlr.document!(%{a: 1})
  """
  ---
  a: 1
  """

  iex> Ymlr.document!({"comment", %{a: 1}})
  """
  ---
  # comment
  a: 1
  """

  iex> Ymlr.document!({["comment 1", "comment 2"], %{"a" => "a", "b" => :b, "c" => "true", "d" => "100"}})
  """
  ---
  # comment 1
  # comment 2
  a: a
  b: b
  c: 'true'
  d: '100'
  """
```

### Encode a multiple documents

```elixir
iex> Ymlr.documents!([%{a: 1}, %{b: 2}])
"""
---
a: 1

---
b: 2
"""
```

## Support for atoms

By default, atoms as map keys are encoded as strings (without the leading
colon). If you want atoms to be encoded with a leading colon in order to be able
to parse it later using [`YamlElixir`'s `atoms`
option](https://hexdocs.pm/yaml_elixir/readme.html#support-for-atoms), you can
pass `atoms: true` as second argument to any of the `Ymlr` module's functions:

```elixir
iex> Ymlr.document!(%{a: 1}, atoms: true)
"""
---
:a: 1
"""
```

### Encode maps with keys sorted

Maps in elixir, implemented by erlang `:maps`, internally are `flatmap`s or `hashmap`s by size.
Large maps will be encoded in strange order.

```elixir
iex> 1..33 |> Map.new(&{&1, &1})|> Ymlr.document!() |> IO.puts
---
4: 4
25: 25
8: 8
...

```

By using `:sort_maps` option, ymlr will encode all entries sorted.

```elixir
iex> 1..33 |> Map.new(&{&1, &1})|> Ymlr.document!(sort_maps: true) |> IO.puts
---
1: 1
2: 2
3: 3
...

```

## Benchmark

This library does not claim to be particularly performant. We do have a script
to benchmark encoding so we know if performance gets better or worse with
changes.

You can find the last Benchmark in [BENCHMARK.md](BENCHMARK.md)

### Running the script

```bash
cd benchmark/
elixir run.exs
```
