<!--
SPDX-FileCopyrightText: 2023 James Harton, Zach Daniel, Alembic Pty and contributors
SPDX-FileCopyrightText: 2023 reactor contributors <https://github.com/ash-project/reactor/graphs.contributors>

SPDX-License-Identifier: MIT
-->

<img src="https://github.com/ash-project/reactor/blob/main/logos/reactor-logo-light-small.png?raw=true#gh-light-mode-only" alt="Logo Light" width="250">
<img src="https://github.com/ash-project/reactor/blob/main/logos/reactor-logo-dark-small.png?raw=true#gh-dark-mode-only" alt="Logo Dark" width="250">

# Reactor

![Elixir CI](https://github.com/ash-project/reactor/actions/workflows/elixir.yml/badge.svg)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
[![Hex version badge](https://img.shields.io/hexpm/v/reactor.svg)](https://hex.pm/packages/reactor)
[![REUSE status](https://api.reuse.software/badge/github.com/ash-project/reactor)](https://api.reuse.software/info/github.com/ash-project/reactor)

Reactor is a dynamic, concurrent, dependency resolving saga orchestrator.

Woah. That's a lot. Let's break it down:

- **Saga orchestrator** A [saga][saga pattern] is a way of providing
  transaction-like semantics across multiple distinct resources.
- **Dependency resolving** reactor allows you to describe the dependencies
  between your saga steps using _arguments_ which are converted into a
  [DAG][dag] and used to compute execution order.
- **Concurrent** unless otherwise specified reactor will run as many steps as
  possible concurrently whilst taking into account the results of the dependency
  resolution.
- **Dynamic** whilst you can define a reactor statically using our awesome DSL,
  you can also build workflows dynamically - and even add steps while the
  reactor is running.

[saga pattern](https://learn.microsoft.com/en-us/azure/architecture/reference-architectures/saga/saga)

## Sponsors

Thanks to [Alembic Pty Ltd](https://alembic.com.au/) for sponsoring a portion of
this project's development.

## Installation

Reactor contains an igniter installer, so if you have igniter installed already you can run `mix igniter.install reactor` to add Reactor to your app.

The package can be installed by adding `reactor` to your list of dependencies in `mix.exs`:

```elixir
def deps do
  [
    {:reactor, "~> 1.0.0"}
  ]
end
```

## Documentation

Our documentation is organized to help you find exactly what you need:

### 🎓 **Learning Reactor** - Tutorials
Step-by-step guides that teach Reactor through hands-on practice:
- **[Getting Started](documentation/tutorials/01-getting-started.md)** - Build your first Reactor
- **[Recursive Execution](documentation/tutorials/05-recursive-execution.md)** - Advanced iterative patterns

### 🔧 **Solving Problems** - How-to Guides
Practical solutions for real-world scenarios:
- **[Payment Processing](documentation/how-to/payment-processing.md)** - E-commerce workflows
- **[Data Pipelines](documentation/how-to/data-pipelines.md)** - ETL orchestration
- **[Testing Strategies](documentation/how-to/testing-strategies.md)** - Testing approaches

### 📚 **API Reference**
Complete technical reference:
- **[DSL Documentation](documentation/dsls/DSL-Reactor.md)** - Complete DSL syntax (auto-generated)
- **[HexDocs](https://hexdocs.pm/reactor)** - Generated API documentation
- **[Latest (main branch)](https://ash-project.github.io/reactor)** - Development docs

### 💡 **Understanding Reactor** - Explanations
Conceptual guides about how and why Reactor works:
- **[Core Concepts](documentation/explanation/concepts.md)** - Sagas, DAGs, compensation
- **[Architecture](documentation/explanation/architecture.md)** - Internal design
- **[Ecosystem](documentation/explanation/ecosystem.md)** - Integration patterns

**Quick Start:** New to Reactor? Start with the [Getting Started tutorial](documentation/tutorials/01-getting-started.md)!

## Contributing

- To contribute updates, fixes or new features please fork and open a
  pull-request against `main`.
- Please use [conventional
  commits](https://www.conventionalcommits.org/en/v1.0.0/) - this allows us to
  dynamically generate the changelog.
- Feel free to ask any questions on the `#reactor` channel on the [Ash
  Discord](https://discord.gg/D7FNG2q).

## Licence

`reactor` is licensed under the terms of the [MIT
license](https://opensource.org/licenses/MIT). See the [`LICENSE` file in this
repository](https://github.com/ash-project/reactor/blob/main/LICENSE)
for details.

[saga pattern]: https://learn.microsoft.com/en-us/azure/architecture/reference-architectures/saga/saga
[dag]: https://en.wikipedia.org/wiki/Directed_acyclic_graph
