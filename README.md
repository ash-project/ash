![Logo](https://github.com/ash-project/ash/blob/main/logos/cropped-for-header-black-text.png?raw=true#gh-light-mode-only)
![Logo](https://github.com/ash-project/ash/blob/main/logos/cropped-for-header-white-text.png?raw=true#gh-dark-mode-only)

![Elixir CI](https://github.com/ash-project/ash/workflows/Ash%20CI/badge.svg)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
[![Hex version badge](https://img.shields.io/hexpm/v/ash.svg)](https://hex.pm/packages/ash)

# Dependency

```elixir
def deps do
  [
    {:ash, "~> 3.0.0-rc.39"}
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

## Links

* ElixirForum: https://elixirforum.com/c/elixir-framework-forums/ash-framework-forum/123
* Official discord Server: https://discord.gg/D7FNG2q
* Ash Framework Twitter: https://twitter.com/AshFramework
* Source: https://github.com/ash-project
* Roadmap: https://github.com/orgs/ash-project/projects/3
* Alembic: https://alembic.com.au/

# Contributors

Ash is made possible by its excellent community!

<a href="https://github.com/ash-project/ash/graphs/contributors">
  <img src="https://contrib.rocks/image?repo=ash-project/ash" />
</a>

[Become a contributor](doumentation/topics/contributing-to-ash.md)
