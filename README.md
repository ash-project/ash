![Logo](https://github.com/ash-project/ash/blob/main/logos/cropped-for-header-black-text.png?raw=true#gh-light-mode-only)
![Logo](https://github.com/ash-project/ash/blob/main/logos/cropped-for-header-white-text.png?raw=true#gh-dark-mode-only)

![Elixir CI](https://github.com/ash-project/ash/workflows/Ash%20CI/badge.svg)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
[![Hex version badge](https://img.shields.io/hexpm/v/ash.svg)](https://hex.pm/packages/ash)

# Dependency

```elixir
def deps do
  [
    {:ash, "~> 2.17.24"}
  ]
end
```

## What is Ash?

Ash Framework is a declarative, resource-oriented application development framework for Elixir. A resource can model anything, like a database table, an external API, or even custom code. Ash provides a rich, and extensive set of tools for interacting with and building on top of these resources. By modeling your application as a set of resources, other tools know exactly how to use them, allowing extensions like `AshGraphql` and `AshJsonApi` to provide top tier APIs with minimal configuration. With filtering/sorting/pagination/calculations/aggregations, pub/sub, policy authorization, rich introspection, and *much* more built-in, and a comprehensive suite of tools to allow you to build your own extensions, the possibilities are endless.

For those familiar with Phoenix, you can think of Ash as a declarative application modeling layer designed to replace your Phoenix contexts.

## Ash Framework 2.0

Ash Framework 2.0 has been released! This begins the official stable release cycle (although it was already quite stable). Thanks to everyone in the community who helped make this possible, from the contributors, the curious, to those already using Ash in prod. I'm eternally grateful for all of your support.

Additionally, I'd like to thank Alembic, who have brought me on to work on Ash full time. Alembic is building complex software with small teams in record time by leveraging tools like Ash Framework and Phoenix LiveView.

Along with the 2.0 release of core, the `AshPostgres`, `AshPhoenix` and `AshArchival` packages have had 1.0 version released as well. `AshGraphql` is next up, and should be released in the next few weeks. Feel free to dive in to it before then, though :).

## Is Ash an alternative to X?

Ash is not meant to be an alternative to Phoenix, Ecto, or Absinthe. Ash uses Ecto under the hood, `AshGraphql` uses Absinthe. Phoenix is absolutely the recommended way to build web interfaces on top of your Ash application (there is a whole package dedicated to it, `AshPhoenix`). Ash is not meant to be the only way that you ever interact with your data, so it is almost a certainty that you will need to use `Ecto` in some cases. For instance, Ash does not currently support bulk actions or atomic updates. For this reason, you can implement custom actions for things that can be encapsulated in your resource, and you have all of Elixir at your disposal to implement custom behavior outside of your resources, with a wide array of escape hatches in between.

## Extensions

### Extensions in 1.0+

- [`AshPostgres`](https://github.com/ash-project/ash_postgres) - Back a resource with postgres. Rich querying capabilities, supporting aggregates, calculations, and fragments. Comes with a migration generator to get you up and running in record time!
- [`AshPhoenix`](https://github.com/ash-project/ash_phoenix) - Helpers to integrate Ash Resources with Phoenix. Tools like `AshPhoenix.Form` allow you to build forms over your resources, and manage complex nested related data with one data structure.
- [`AshArchival`](https://github.com/ash-project/ash_archival) - A tiny but powerful extension. Get archival (A.K.A soft deletion) with one line of code.

### Extensions <1.0

* [`AshGraphql`](https://github.com/ash-project/ash_graphql) - Create a GraphQL from your resources with only a few lines of code. Backed by the excellent Absinthe library. It comes with its own fully implemented dataloader, and automatically derives all the types, fields, and mutations automatically. Getting a relay compatible GraphQL API is as easy as setting the `relay?` toggle.
- [`AshJsonApi`](https://github.com/ash-project/ash_json_api) - Create a JSON:API spec compliant API in minutes.
- [`AshAdmin`](https://github.com/ash-project/ash_admin) - A rich admin UI automatically derived from your resource definitions.
- [`AshCsv`](https://github.com/ash-project/ash_csv) - Back your resource with a CSV file.
- [`Spark`](https://github.com/ash-project/spark) - The core declarative DSL library that backs Ash and its extensions.

### Unreleased Extensions

- [`AshPaperTrail`](https://github.com/ash-project/ash_paper_trail) - Creates and manages a versions table for a resource, and writes all changes to that version resource. With one line of code.
- [`AshJsonApiWrapper`](https://github.com/ash-project/ash_json_api_wrapper) - Back your resource with an external API using finch and configuration to describe how your resource maps to the response from the external service.

### Your Own Extensions

All of the extensions above are created with a set of tools that are free to use to create your own extensions. They can all be used as a basis point, or as inspiration. Many users have created their own extensions for various reasons. An extension can both add to the resource's DSL and programatically restructure the resource. For example, `AshArchival` adds an attribute, modifies every destroy action, and adds a "base filter" to the resource. This allows for extremely powerful extensions.

## Links

* ElixirForum: https://elixirforum.com/c/elixir-framework-forums/ash-framework-forum/123
* New and improved docs:  https://ash-hq.org!
* Office hours/Q&A 2.0 release live stream: https://www.youtube.com/watch?v=BchANMO1f8s
* Official discord Server: https://discord.gg/D7FNG2q
* Sponsors Dashboard: https://github.com/sponsors/zachdaniel/
* Ash Framework Twitter: https://twitter.com/AshFramework
* Source: https://github.com/ash-project
* Roadmap: https://github.com/orgs/ash-project/projects/3
* Alembic: https://alembic.com.au/

# Contributors

Ash is made possible by its excellent community!

<a href="https://github.com/ash-project/ash/graphs/contributors">
  <img src="https://contrib.rocks/image?repo=ash-project/ash" />
</a>

[Become a contributor](https://ash-hq.org/docs/guides/ash/latest/how_to/contribute.md)
