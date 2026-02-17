<img src="https://github.com/ash-project/iterex/blob/main/logos/iterex-logo-small.png?raw=true#gh-light-mode-only" alt="Logo Light" width="250">
<img src="https://github.com/ash-project/iterex/blob/main/logos/iterex-logo-small.png?raw=true#gh-dark-mode-only" alt="Logo Dark" width="250">

# Iterex

![Elixir CI](https://github.com/ash-project/iterex/actions/workflows/elixir.yml/badge.svg)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
[![Hex version badge](https://img.shields.io/hexpm/v/iterex.svg)](https://hex.pm/packages/iterex)

Iterex is a library that provides external iterators for Elixir collections.

Iterators provide the flexibility of `Enum` with the laziness of `Stream` and the ability to pause and resume iteration.

The `Iter` module provides the public interface to working with iterators, which wraps an `Iter.Iterable` (to make it easier to pattern match, etc). You'll find most of the functions you'd want from `Stream` and `Enum` provided by this module, but often with different return values to enable you to resume iteration where possible. The `Enumerable` and `Collectable` protocols have been implemented for `Iter` so you can use them as drop in replacements for other collection types where needed.

Some differences from `Enum` and `Stream`:

- `Iter.next/1` - the core advantage of iterators over streams. Allows you to retrieve the next element from an iterator and a new iterator.
- `Iter.prepend/2`, `Iter.append/2` and `Iter.peek/1..2` - iterators can be easily composed allowing features that might otherwise break `Stream` semantics.

See the [documentation on hexdocs](https://hexdocs.pm/iterex) for more information.

## Sponsors

Thanks to [Alembic Pty Ltd](https://alembic.com.au/) for sponsoring a portion of
this project's development.

## Installation

The package can be installed by adding `iterex` to your list of dependencies in `mix.exs`:

```elixir
def deps do
  [
    {:iterex, "~> 0.1.2"}
  ]
end
```

## Contributing

- To contribute updates, fixes or new features please fork and open a pull-request against `main`.
- Please use [conventional commits](https://www.conventionalcommits.org/en/v1.0.0/) - this allows us to dynamically generate the changelog.

## Licence

`iterex` is licensed under the terms of the [MIT license](https://opensource.org/licenses/MIT). See the [`LICENSE` file in this repository](https://github.com/ash-project/iterex/blob/main/LICENSE)
for details.
