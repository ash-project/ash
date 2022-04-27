![Logo](https://github.com/ash-project/ash/blob/main/logos/cropped-for-header.png?raw=true)
![Elixir CI](https://github.com/ash-project/ash/workflows/Ash%20CI/badge.svg)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
[![Hex version badge](https://img.shields.io/hexpm/v/ash.svg)](https://hex.pm/packages/ash)

## Documentation

All documentation is contained in the generated hex documentation located [here](https://hexdocs.pm/ash). Head there for installation and usage information. What follows is only a brief introduction to Ash.

## Beta NOTICE

Ash is in beta. The package version is 1.0.0+, and most of the time that means stable, but in this case it _does not_. The 2.0 release will be the stable release.

With that said, we are getting closer and closer to a non-beta release. The primary things that are being worked on to that end are documentation, some additional testing, and various interface improvements that are very likely to be backwards compatible. Ash is being used in production currently. We simply hold the project to a high standard when it comes to taking it out of beta.

# Dependency

```elixir
def deps do
  [
    {:ash, "~> 1.29.0-rc0"}
  ]
end
```

# Links

## Guides

- [Getting Started Tutorial](https://hexdocs.pm/ash/getting_started.html)
- [Resource DSL Documentation](https://hexdocs.pm/ash/Ash.Resource.Dsl.html)
- [API DSL Documentation](https://hexdocs.pm/ash/Ash.Api.Dsl.html)
- [API interface documentation](https://hexdocs.pm/ash/Ash.Api.html)
- [Query Documentation](https://hexdocs.pm/ash/Ash.Query.html)
- [Changeset Documentation](https://hexdocs.pm/ash/Ash.Changeset.html)
- [Example Application](https://github.com/ash-project/ash_example)

## Extensions

### APIs

- [AshJsonApi](https://hexdocs.pm/ash_json_api)
- [AshGraphql (beta)](https://hexdocs.pm/ash_graphql)

### Authorizers

- [AshPolicyAuthorizer](https://hexdocs.pm/ash_policy_authorizer)

### Datalayers

- [AshPostgres](https://hexdocs.pm/ash_postgres)
- [AshCsv](https://hexdocs.pm/ash_csv)
- [Ets (built-in)](https://hexdocs.pm/ash/Ash.DataLayer.Ets.html) - Only used for testing
- [Mnesia (built-in)](https://hexdocs.pm/ash/Ash.DataLayer.Mnesia.html)

## Introduction

Traditional MVC Frameworks (Rails, Django, .Net, Phoenix, etc) leave it up to the user to build the glue between requests for data (HTTP requests in various forms as well as server-side domain logic) and their respective ORMs. In that space, there is an incredible amount of boilerplate code that must get written from scratch for each application (authentication, authorization, sorting, filtering, sideloading relationships, serialization, etc).

Ash is an opinionated yet configurable framework designed to reduce boilerplate in an Elixir application. Ash does this by providing a layer of abstraction over your system's data layer(s) with `Resources`. It is designed to be used in conjunction with a phoenix application, or on its own.

To riff on a famous JRR Tolkien quote, a `Resource` is "One Interface to rule them all, One Interface to find them" and will become an indispensable place to define contracts for interacting with data throughout your application.

To start using Ash, first declare your `Resources` using the Ash `Resource` DSL. You could technically stop there, and just leverage the Ash Elixir API to avoid writing boilerplate. More likely, you would use extensions like Ash.JsonApi or Ash.GraphQL with Phoenix to add external interfaces to those resources without having to write any extra code at all.

Ash is an open-source project and draws inspiration from similar ideas in other frameworks and concepts. The goal of Ash is to lower the barrier to adopting and using Elixir and Phoenix, and in doing so help these amazing communities attract new developers, projects, and companies.

## Example Resource

```elixir
defmodule Post do
  use Ash.Resource

  actions do
    read :read

    create :create
  end

  attributes do
    attribute :name, :string
  end

  relationships do
    belongs_to :author, Author
  end
end
```

See the [Getting Started Tutorial](https://hexdocs.pm/ash/getting_started.html) for more information.

For those looking to add Ash extensions:

- see `Ash.Dsl.Extension` for adding configuration.
- If you are looking to write a new data source, also see the `Ash.DataLayer` documentation.
- If you are looking to write a new authorizer, see `Ash.Authorizer`
- If you are looking to write a "front end", something powered by Ash resources, a guide on
  building those kinds of tools is in the works.

## Creating a new release of Ash

- check out the repository locally
- run `mix git_ops.release` (see git_ops documentation for more information)
- check the changelog/new release number
- push (with tags) and CI will automatically deploy the hex package

# Contributors

Ash is made possible by its excellent community!

[![zachdaniel](https://avatars.githubusercontent.com/u/5722339?v=4&s=117)](https://github.com/zachdaniel)[![andrewcallahan](https://avatars.githubusercontent.com/u/529744?v=4&s=117)](https://github.com/andrewcallahan)[![zimt28](https://www.gravatar.com/avatar/b515a681d8a803aad9f23986a4ab6398de348ed1?d=identicon&s=117)](https://github.com/zimt28)[![totaltrash](https://avatars.githubusercontent.com/u/637350?v=4&s=117)](https://github.com/totaltrash)[![axelson](https://avatars.githubusercontent.com/u/9973?v=4&s=117)](https://github.com/axelson)[![vbrazo](https://avatars.githubusercontent.com/u/1292556?v=4&s=117)](https://github.com/vbrazo)

[![mario-mazo](https://avatars.githubusercontent.com/u/30439204?v=4&s=117)](https://github.com/mario-mazo)[![vherr2](https://avatars.githubusercontent.com/u/3813665?v=4&s=117)](https://github.com/vherr2)[![TheFirstAvenger](https://avatars.githubusercontent.com/u/8557871?v=4&s=117)](https://github.com/TheFirstAvenger)[![kernel-io](https://avatars.githubusercontent.com/u/1523960?v=4&s=117)](https://github.com/kernel-io)[![kingshalaby1](https://avatars.githubusercontent.com/u/60473021?v=4&s=117)](https://github.com/kingshalaby1)[![frankdugan3](https://avatars.githubusercontent.com/u/10977914?v=4&s=117)](https://github.com/frankdugan3)

[![mangeption](https://avatars.githubusercontent.com/u/13043330?v=4&s=117)](https://github.com/mangeption)[![michaelst](https://avatars.githubusercontent.com/u/4080508?v=4&s=117)](https://github.com/michaelst)[![savish](https://avatars.githubusercontent.com/u/1764878?v=4&s=117)](https://github.com/savish)[![alexfreska](https://avatars.githubusercontent.com/u/1412796?v=4&s=117)](https://github.com/alexfreska)[![ChristianTovar](https://avatars.githubusercontent.com/u/13787741?v=4&s=117)](https://github.com/ChristianTovar)[![dkuku](https://avatars.githubusercontent.com/u/904179?v=4&s=117)](https://github.com/dkuku)

[![doawoo](https://avatars.githubusercontent.com/u/61982076?v=4&s=117)](https://github.com/doawoo)[![floriank](https://avatars.githubusercontent.com/u/498241?v=4&s=117)](https://github.com/floriank)[![janpieper](https://avatars.githubusercontent.com/u/426371?v=4&s=117)](https://github.com/janpieper)[![elbow-jason](https://avatars.githubusercontent.com/u/4923601?v=4&s=117)](https://github.com/elbow-jason)[![maartenvanvliet](https://avatars.githubusercontent.com/u/54566?v=4&s=117)](https://github.com/maartenvanvliet)[![florius0](https://avatars.githubusercontent.com/u/18403735?v=4&s=117)](https://github.com/florius0)

[![WolfDan](https://avatars.githubusercontent.com/u/5377526?v=4&s=117)](https://github.com/WolfDan)[![mhussa](https://www.gravatar.com/avatar/mhussa?d=identicon&s=117)](https://github.com/mhussa)
