# Ash

Ash is an Elixir framework designed to work with Phoenix. At it's core is a concept called a `Resource` which enables developers to declaritively define modules of an entity (such as a database table) and in doing so automatically create a public API for that entity that be accessed and transmitted in many forms such as JSONAPI, GraphQL, LiveView, or just within Elixir code elsewhere in a Phoenix app with very little configuration.

Developers shoud be focusing on their core business logic - not boilerplate code such as filtering, pagination, serializing, and sideloading relational data. Yet seemingly everytime a new Phoenix app is created all this concepts need to get reinvented or brought in piecemeal. This takes substantial time and money and is highly inefficient.

Ash builds upon the incredible power of Phoenix and empowers developers to get up and running with a fully functional app in substantially less time, while still being flexible enough to allow customization when it inevitably comes up.

Ash is an open source project, and draws inspiration from similar ideas in other frameworks and concepts. The goal of Ash is to lower the barrier to adopting and using Elixir and Phoenix, and in doing so help these amazing communities attract new develpers, projects, and companies.

## Installation

If [available in Hex](https://hex.pm/docs/publish), the package can be installed
by adding `ash` to your list of dependencies in `mix.exs`:

```elixir
def deps do
  [
    {:ash, "~> 0.1.0"}
  ]
end
```

Documentation can be generated with [ExDoc](https://github.com/elixir-lang/ex_doc)
and published on [HexDocs](https://hexdocs.pm). Once published, the docs can
be found at [https://hexdocs.pm/ash](https://hexdocs.pm/ash).

