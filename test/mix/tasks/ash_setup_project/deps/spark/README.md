<!--
SPDX-FileCopyrightText: 2020 Zach Daniel
SPDX-FileCopyrightText: 2022 spark contributors <https://github.com/ash-project/spark/graphs.contributors>

SPDX-License-Identifier: MIT
-->

<p align="center">
  <img src="logos/logo.svg" alt="Spark Logo" width="150" height="200" />
</p>

<!-- ex_doc_ignore_start -->
# Spark
<!-- ex_doc_ignore_end -->

[![Spark CI](https://github.com/ash-project/spark/actions/workflows/elixir.yml/badge.svg)](https://github.com/ash-project/spark/actions/workflows/elixir.yml)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
[![Hex version badge](https://img.shields.io/hexpm/v/spark.svg)](https://hex.pm/packages/spark)
[![Hexdocs badge](https://img.shields.io/badge/docs-hexdocs-purple)](https://hexdocs.pm/spark)
[![REUSE status](https://api.reuse.software/badge/github.com/ash-project/spark)](https://api.reuse.software/info/github.com/ash-project/spark)
[![Ask DeepWiki](https://deepwiki.com/badge.svg)](https://deepwiki.com/ash-project/spark)

**Build powerful, extensible DSLs with exceptional developer experience**

Spark is a framework for creating declarative domain-specific languages in
Elixir. It transforms simple struct definitions into rich, extensible DSLs that
come with autocomplete, documentation generation, and sophisticated tooling
built right in.

## Quick Example

Here's how you can build a data validator DSL with Spark:

```elixir
defmodule MyApp.PersonValidator do
  use MyLibrary.Validator

  fields do
    required [:name]
    field :name, :string

    field :email, :string do
      check &String.contains?(&1, "@")
      transform &String.trim/1
    end
  end
end

MyApp.PersonValidator.validate(%{name: "Zach", email: " foo@example.com "})
{:ok, %{name: "Zach", email: "foo@example.com"}}
```

The DSL definition itself is clean and declarative:

```elixir
@field %Spark.Dsl.Entity{
  name: :field,
  args: [:name, :type],
  target: Field,
  describe: "A field that is accepted by the validator",
  schema: [
    name: [type: :atom, required: true, doc: "The name of the field"],
    type: [type: {:one_of, [:integer, :string]}, required: true, doc: "The type of the field"],
    check: [type: {:fun, 1}, doc: "A function to validate the value"],
    transform: [type: {:fun, 1}, doc: "A function to transform the value"]
  ]
}

@fields %Spark.Dsl.Section{
  name: :fields,
  entities: [@field],
  describe: "Configure the fields that are supported and required"
}

use Spark.Dsl.Extension, sections: [@fields]
```

## What You Get Out of the Box

* 🔧 **Extensible Architecture** - Anyone can write extensions for your DSL,
  making it infinitely customizable
* 🧠 **Smart Autocomplete** - Built-in ElixirSense integration provides
  intelligent code completion and inline documentation in your editor
* 📚 **Auto Documentation** - Generate comprehensive documentation for your DSL
  automatically, including all options and usage examples
* ⚡ **Developer Tools** - Mix tasks for formatting, code generation, and
  maintaining `locals_without_parens` automatically
* 🔄 **Compile-time Processing** - Use transformers to modify DSL structure$
  during compilation and verifiers to validate correctness
* 🎯 **Type Safety** - Rich schema validation ensures DSL usage is correct at
  compile time with helpful error messages
* 🔍 **Introspection** - Built-in tools to inspect and query DSL definitions
  programmatically at runtime

## Installation

Add `spark` to your list of dependencies in `mix.exs`:

```elixir
def deps do
  [
    {:spark, "~> 2.3"}
  ]
end
```

## Getting Started

The best way to get started is with our comprehensive tutorial that walks you
through building a complete DSL from scratch:

📖 **[Get Started with Spark](documentation/tutorials/get-started-with-spark.md)** -
Build a data validator DSL step by step

### Quick Start Checklist

1. **Define your DSL structure** using `Spark.Dsl.Section` and `Spark.Dsl.Entity`
2. **Create your extension** with `use Spark.Dsl.Extension`
3. **Build your DSL module** that users will import
4. **Add transformers and verifiers** for advanced behavior
5. **Generate helper functions** with `Spark.InfoGenerator`

Each step is covered in detail in the tutorial above.

## Documentation

### 📚 Guides & Tutorials
- **[Get Started with Spark](documentation/tutorials/get-started-with-spark.md)** -
  Complete tutorial building a validator DSL
- **[Writing Extensions](documentation/how_to/writing-extensions.md)** -
  Deep dive into extension development
- **[Setup Autocomplete](documentation/how_to/setup-autocomplete.md)** -
  Configure editor integration
- **[Split Up Large DSLs](documentation/how_to/split-up-large-dsls.md)** -
  Organize complex DSL definitions
- **[Use Source Annotations](documentation/how_to/use-source-annotations.md)** -
  Leverage location tracking for better errors

### 🔧 API Reference
- **[HexDocs](https://hexdocs.pm/spark)** - Complete API documentation
- **Core Modules**: `Spark.Dsl.Extension`, `Spark.Dsl.Entity`,
  `Spark.Dsl.Section`
- **Advanced Features**: `Spark.Dsl.Transformer`, `Spark.Dsl.Verifier`,
  `Spark.InfoGenerator`

## Production Ready

Spark is battle-tested and powers all DSLs in the [Ash Framework](https://ash-hq.org),
handling complex real-world applications with thousands of DSL definitions.
Whether you're building configuration DSLs, workflow orchestrators, or
domain-specific languages for your business logic, Spark provides the foundation
for production-grade solutions.

<!-- ex_doc_ignore_start -->
## Contributing

We welcome contributions! Please see our [contributing guidelines](CONTRIBUTING.md)
and feel free to open issues or submit pull requests.
<!-- ex_doc_ignore_end -->

## Links

- **[GitHub](https://github.com/ash-project/spark)** - Source code and issue
  tracking
- **[Hex.pm](https://hex.pm/packages/spark)** - Package repository
- **[HexDocs](https://hexdocs.pm/spark)** - API documentation
- **[Ash Framework](https://ash-hq.org)** - See Spark in action
- **[Discord](https://discord.gg/HTHRaaVPUc)** - Community chat
- **[Forum](https://elixirforum.com/c/elixir-framework-forums/ash-framework-forum)** -
  Discussion forum

<!-- ex_doc_ignore_start -->
## License

MIT - see [`LICENSES/MIT.txt`](LICENSES/MIT.txt) for details.
<!-- ex_doc_ignore_end -->
