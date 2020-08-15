![Logo](https://github.com/ash-project/ash/blob/master/logos/cropped-for-header.png)

![Elixir CI](https://github.com/ash-project/ash/workflows/Ash%20CI/badge.svg)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
[![Coverage Status](https://coveralls.io/repos/github/ash-project/ash/badge.svg?branch=master)](https://coveralls.io/github/ash-project/ash?branch=master)
[![Hex version badge](https://img.shields.io/hexpm/v/ash.svg)](https://hex.pm/packages/ash)

## ALPHA NOTICE

Ash is in alpha. The package version is 1.0.0+, and most of the time that means stable, but in this case it _does not_. The 2.0 release will be the stable release.

```
def deps do
  [
    {:ash, "~> 1.6.3"}
  ]
end
```

## Documentation

All documentation is contained in the generated hex documentation located [here](https://hexdocs.pm/ash). Head there for installation and usage information. What follows is only a brief introduction to Ash.

## Introduction

Traditional MVC Frameworks (Rails, Django, .Net, Phoenix, etc) leave it up to the user to build the glue between requests for data (HTTP requests in various forms as well as server-side domain logic) and their respective ORMs. In that space, there is an incredible amount of boilerplate code that must get written from scratch for each application (authentication, authorization, sorting, filtering, sideloading relationships, serialization, etc).

Ash is an opinionated yet configurable framework designed to reduce boilerplate in an Elixir application. Ash does this by providing a layer of abstraction over your system's data layer(s) with `Resources`. It is designed to be used in conjunction with a phoenix application, or on its own.

To riff on a famous JRR Tolkien quote, a `Resource`is "One Interface to rule them all, One Interface to find them" and will become an indispensable place to define contracts for interacting with data throughout your application.

To start using Ash, first declare your `Resources` using the Ash `Resource` DSL. You could technically stop there, and just leverage the Ash Elixir API to avoid writing boilerplate. More likely, you would use extensions like Ash.JsonApi or Ash.GraphQL with Phoenix to add external interfaces to those resources without having to write any extra code at all.

Ash is an open-source project and draws inspiration from similar ideas in other frameworks and concepts. The goal of Ash is to lower the barrier to adopting and using Elixir and Phoenix, and in doing so help these amazing communities attract new developers, projects, and companies.

## Creating a new release of Ash

- check out the repository locally
- run `mix git_ops.release` (see git_ops documentation for more information)
- check the changelog/new release number
- push (with tags) and CI will automatically deploy the hex package
