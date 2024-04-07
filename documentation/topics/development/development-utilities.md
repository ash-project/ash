# Development Utilities

## Formatting DSLs

All Ash packages that ship with extensions provide exports in their `.formatter.exs`. This prevents the formatter from turning, for example, `attribute :name, :string` into `attribute(:name, :string)`. To enable this, add `:ash` (and any other Ash libraries you are using) to your `.formatter.exs` file:

```elixir
[
  # ...
  import_deps: [..., :ash],
  # ...
]
```

## ElixirSense Plugin

Ash uses [Spark](https://hexdocs.pm/spark) to build all of our DSLs (like `Ash.Resource` and `Ash.Domain`) and to validate options lists to functions. `Spark` ships with an extension that is automatically picked up by ElixirLS to provide autocomplete for all of our DSLs, and options list. You don't need to do anything to enable this, but it only works with ElixirLS (not other language server tools).

## Formatter plugin

`Spark` also ships with a formatter plugin that can help you keep your resources formatted consistently. This plugin can sort the sections of your DSL to make your resources more consistent, and it can remove any accidentally added parentheses around DSL code.

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
  "Ash.Domain": [],
  "Ash.Resource": [
    section_order: [
      # any section not in this list is left where it is
      # but these sections will always appear in this order in a resource
      :actions,
      :attributes,
      :relationships,
      :identities
    ]
  ]
```

#### If you `use` a different module than Ash.Resource

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
