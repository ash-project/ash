<!--
SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>

SPDX-License-Identifier: MIT
-->

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

## Ecto Compatibility Checker

Ash resources generate Ecto schemas automatically, but there can be differences between how Ash handles defaults and timestamps versus how Ecto handles them. The `mix ash.ecto_compat` task helps you identify these differences.

### Usage

Check all resources in your configured domains:

```bash
mix ash.ecto_compat
```

Check a specific resource:

```bash
mix ash.ecto_compat --resource MyApp.User
```

Check all resources in a specific domain:

```bash
mix ash.ecto_compat --domain MyApp.Domain
```

### What It Checks

The tool identifies two types of compatibility issues:

1. **Missing Autogenerate Fields**: Ash resources with `create_timestamp` or `update_timestamp` attributes that are not configured as autogenerate fields in the generated Ecto schema. This means using `Repo.insert/1` or `Repo.update/1` directly won't automatically populate these timestamps.

2. **Default Mismatches**: Ash attributes with static defaults that don't appear in the Ecto struct. This means creating a struct directly (`%MyResource{}`) or using `Repo.insert/1` won't include these defaults.

### Example Output

```
⚠️  Ecto Compatibility Warnings:

🔴 Missing Autogenerate Fields
   Resource: MyApp.User
   Fields: [:inserted_at, :updated_at]
   Details: Ash timestamps [:inserted_at, :updated_at] exist but are not in MyApp.User.__schema__(:autogenerate_fields) = []

🟡 Default Mismatch
   Resource: MyApp.Post
   Details: Ash defaults are not visible on the bare struct of MyApp.Post. Using Repo.insert/1 may differ from Ash.create/2.
     * version: Ash default = 1, struct default = nil
```

### What To Do About Warnings

If you see warnings, you have two options:

1. **Use Ash API instead of direct Repo calls**: Instead of `Repo.insert!(%MyResource{})`, use `Ash.create!/2` to ensure defaults and timestamps are handled correctly.

2. **Fix the Ecto schema generation**: This would require changes to how Ash generates Ecto schemas. The `mix ash.ecto_compat` tool helps identify where these differences exist so you can make informed decisions about whether to use Ash API calls or direct Repo calls.

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
