# Development Utilities

## ElixirSense Plugin

The Ash ElixirSense plugin offers custom auto complete inside of any Ash DSL module (i.e resource/api/flow/registry)

With the release of ElixirLS 0.10.0, there is only one requirement to make this work for your project, which is to add `elixir_sense`
as a dev dependency. We're exploring ways to remove this requirement in the future so that it works with entirely automatically.

`{:elixir_sense, github: "elixir-lsp/elixir_sense", only: [:dev, :test]}`

## Resource Formatter

Ash resources often have a lot of DSL sections, and in order to increase the consistency of your resources, Ash ships with an elixir formatter plugin that allows for declaring a static section order and ensuring all resources honor it.

### Connecting to the formatter

Add the following to your `.formatter.exs`

```elixir
[
  plugins: [Ash.ResourceFormatter], # <- add the plugin here
  inputs: ...
]
```

### Configuration

```elixir
config :ash, :formatter,
  # If you have a custom module that you use instead of `Ash.Resource` (a.k.a a "base resource"), you can configure it here.
  using_modules: [Ash.Resource, MyApp.Resource],
  # these sections will always appear in this order. Any other sections may still appear before/after/around them, so you typically want
  # to include an order for every section that may appear in your resources.
  section_order: [
    :resource,
    :identities,
    :attributes,
    :relationships,
    ...
  ]
```
