# Alternatives

There aren't really any alternatives to Ash that we are aware of that do all of the same things, but there are many different packages out there that do some of the things that Ash does.

This is a living document, and is not comprehensive. We are not *vouching* for any of these packages, but rather listing them here for your convenience to investigate on your own.

Want to add or edit this list? Open a [pull request](https://github.com/ash-project/ash/edit/main/documentation/topics/about_ash/alternatives.md) Want a more comprehensive list? Check out the [Awesome Elixir](https://github.com/h4cc/awesome-elixir).

## Application Frameworks

These frameworks have similar overarching goals of helping you build your application layer.

- [Commanded](https://hexdocs.pm/commanded) - An event sourced application framework.
- [Sleeky](https://hexdocs.pm/sleeky) - Billed as a lightweight alternative to Ash. Inspired by Ash, but more tightly built on top of ecto.

## Application Design

- [Boundary](https://hexdocs.pm/boundary) - A library for defining boundaries in your application.
- [Phoenix Contexts](https://hexdocs.pm/phoenix/contexts.html) - Phoenix ships with a concept called "contexts", which provide some generators with application design guidance.

## Building APIs

- [Absinthe](https://hexdocs.pm/absinthe) - A GraphQL toolkit for Elixir. This is what `AshGraphql` uses under the hood, but you can also use Absinthe directly.
- [Phoenix](https://hexdocs.pm/phoenix) - Phoenix is a web framework for Elixir. It is not necessarily an API framework, but has all the tools you need to build APIs by hand.
- [JSONAPI Elixir](https://hexdocs.pm/jsonapi) - A library for building JSONAPI compliant APIs in Elixir on top of Ecto.
- [Open API Spex](https://github.com/open-api-spex) - A library for generating OpenAPI (Swagger) documentation for your API. We generate these for you in AshJsonApi, but you can use this library to build open api specifications of your hand-written API

## Working with Data

- [Ecto](https://hexdocs.pm/ecto) - Ecto is a database wrapper and query generator for Elixir. In many cases, Ash uses Ecto under the hood, but it is also available to build on top of directly.
- [Flop](https://hexdocs.pm/flop) - A library designed to easily apply filtering, ordering, and pagination to Ecto queries.

## Authentication

- [mix phx.gen.auth](https://hexdocs.pm/phoenix/Mix.Tasks.Phx.Gen.Auth.html) - A mix task that generates authentication for a Phoenix application. Some folks prefer to use this over `AshAuthentication` even if they are using Ash.
- [Assent](https://hexdocs.pm/assent) - Multi-provider authentication framework.

## Authorization

- [Bodyguard](https://hexdocs.pm/bodyguard) - A phoenix-context-based policy authorization framework.

## Validation

- [Ecto](https://hexdocs.pm/ecto) - Ecto can be used to validate data at the edge, using things like schema-less changesets.
- [Drops](https://hexdocs.pm/drops) - a collection of small modules that provide useful extensions and functions that can be used to work with data effectively.
- [GuardedStruct](https://github.com/mishka-group/mishka_developer_tools/blob/master/guidance/guarded-struct.md) - validation, sanitization, and construction of structs, supporting nested structs.
