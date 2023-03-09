# Development Utilities

## ElixirSense Plugin

The Ash ElixirSense plugin offers custom auto complete inside of any Ash DSL module (i.e resource/api/flow/registry)

As of this writing, this does not work with the currently released VSCode package. We are waiting for them to do another release
to resolve this issue. However, you can clone down the elixir-ls repository, run its release command, and configure VSCode to point
at the folder where you did that.

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

#### Minimal config for your Ash Resources

```elixir
config :spark, :formatter,
  remove_parens?: true,
  "Ash.Resource": [
    type: Ash.Resource,
    section_order: [
      :authentication,
      :token,
      :attributes,
      :relationships,
      :policies,
      :postgres
    ]
  ]
```

#### If you use a different module than Ash.Resource

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
