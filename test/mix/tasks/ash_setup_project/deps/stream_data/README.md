# StreamData

[![hex.pm badge](https://img.shields.io/badge/Package%20on%20hex.pm-informational)](https://hex.pm/packages/stream_data)
[![CI](https://github.com/whatyouhide/stream_data/workflows/CI/badge.svg)](https://github.com/whatyouhide/stream_data/actions/workflows/main.yml)
[![Coverage Status](https://coveralls.io/repos/github/whatyouhide/stream_data/badge.svg?branch=master)](https://coveralls.io/github/whatyouhide/stream_data?branch=master)

> StreamData is an Elixir library for **data generation** and **property-based testing**.

Read [the announcement on the Elixir website](https://elixir-lang.org/blog/2017/10/31/stream-data-property-based-testing-and-data-generation-for-elixir/).

## Installation

Add `stream_data` to your list of dependencies:

```elixir
defp deps() do
  [{:stream_data, "~> 1.0", only: :test}]
end
```

and run `mix deps.get`. StreamData is usually added only to the `:test` environment since it's used in tests and test data generation.
To also import StreamData's formatter configuration, add the `:dev`  environment as well as `:test` for `stream_data` and add `:stream_data` to your `.formatter.exs`:

```elixir
[
  import_deps: [:stream_data]
]
```

## Usage

[The documentation is available online.](https://hexdocs.pm/stream_data/)

StreamData is made of two main components: data generation and property-based testing. The `StreamData` module provides tools to work with data generation. The `ExUnitProperties` module takes care of the property-based testing functionality.

### Data generation

All data generation functionality is provided in the `StreamData` module. `StreamData` provides "generators" and functions to combine those generators and create new ones. Since generators implement the `Enumerable` protocol, it's easy to use them as infinite streams of data:

```elixir
StreamData.integer() |> Stream.map(&abs/1) |> Enum.take(3)
#=> [1, 0, 2]
```

`StreamData` provides all the necessary tools to create arbitrarily complex custom generators:

```elixir
require ExUnitProperties

domains = [
  "gmail.com",
  "hotmail.com",
  "yahoo.com",
]

email_generator =
  ExUnitProperties.gen all name <- StreamData.string(:alphanumeric),
                           name != "",
                           domain <- StreamData.member_of(domains) do
    name <> "@" <> domain
  end

Enum.take(StreamData.resize(email_generator, 20), 2)
#=> ["efsT6Px@hotmail.com", "swEowmk7mW0VmkJDF@yahoo.com"]
```

### Property testing

Property testing aims at randomizing test data in order to make tests more robust. Instead of writing a bunch of inputs and expected outputs by hand, with property-based testing we write a *property* of our code that should hold for a set of data, and then we generate data in this set, in attempt to falsify that property. To generate this data, we can use the above-mentioned `StreamData` module.

```elixir
use ExUnitProperties

property "bin1 <> bin2 always starts with bin1" do
  check all bin1 <- binary(),
            bin2 <- binary() do
    assert String.starts_with?(bin1 <> bin2, bin1)
  end
end
```

To know more about property-based testing, read the `ExUnitProperties` documentation. Another great resource about property-based testing in Erlang (but with most ideas that apply to Elixir as well) is Fred Hebert's website [propertesting.com](http://propertesting.com).

The property-based testing side of this library is heavily inspired by the [original QuickCheck paper](http://www.cs.tufts.edu/~nr/cs257/archive/john-hughes/quick.pdf) (which targeted Haskell) as well as Clojure's take on property-based testing, [test.check](https://github.com/clojure/test.check).

## Differences from other property-based testing frameworks

There are a handful of property-based testing frameworks for the BEAM ecosystem (Erlang, Elixir, and so on). For Elixir, the main alternative to StreamData is [PropCheck](https://github.com/alfert/propcheck). PropCheck is a wrapper around [PropEr](https://github.com/proper-testing/proper), which is a property-based testing framework for Erlang. There are a few fundamental differences between StreamData and PropEr. They are listed below to help you choose between the two.

**PropEr** (via PropCheck):

  * It provides *stateful property-based testing*. If you need to test a system with state by building a model of the system to test against, you'll have to go with PropCheck since StreamData doesn't support this yet.

  * It can store counter-examples: StreamData doesn't support storing counter-examples in a file (you have to reuse the seed that caused the failure in order to reproduce it).

**StreamData**:

  * Provides functionality for generating data as the base for property-based testing. StreamData generators can be used outside of property-based testing as normal Elixir streams that produce random data.

  * It is native to Elixir. It's written entirely in Elixir and has an idiomatic Elixir API (for example, all generators are Elixir enumerables).

## License

Copyright 2017 Andrea Leopardi and José Valim

  Licensed under the Apache License, Version 2.0 (the "License");
  you may not use this file except in compliance with the License.
  You may obtain a copy of the License at

      http://www.apache.org/licenses/LICENSE-2.0

  Unless required by applicable law or agreed to in writing, software
  distributed under the License is distributed on an "AS IS" BASIS,
  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
  See the License for the specific language governing permissions and
  limitations under the License.
