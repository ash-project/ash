# Development Utilities

## ElixirSense Plugin

The Ash ElixirSense plugin offers custom auto complete inside of any Ash DSL module (i.e resource/api/flow/registry)

With the release of ElixirLS 0.10.0, there is only one requirement to make this work for your project, which is to add `elixir_sense`
as a dev dependency. We're exploring ways to remove this requirement in the future so that it works with entirely automatically.

`{:elixir_sense, github: "elixir-lsp/elixir_sense", only: [:dev, :test]}`

## Formatter plugin

The underlying DSL tooling `Spark` has a formatter plugin that can help you keep your resources consistent and neat.

### Adding the plugin

Add the following to your `.formatter.exs`

```elixir
[
  plugins: [Spark.Formatter], # <- add the plugin here
  inputs: ...
]
```

### Configuration

```elixir
config :spark, :formatter,
  [
    "Ash.Resource": [
      section_order: [
        :resource,
        :identities,
        :attributes,
        :relationships,
        ...
      ]
    ],
    # If you use a different module than Ash.Resource
    "MyApp.Resource": [
      type: Ash.Resource,
      # What extensions might be added by your base module
      extensions: [...],
      section_order: [
        :resource,
        :identities,
        :attributes,
        :relationships,
        ...
      ]
    ]
  ]
```
