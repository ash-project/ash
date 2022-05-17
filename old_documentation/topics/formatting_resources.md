# Resource Formatting

Each extension has its own formatting configuration for the extension that it creates. You'll want to update your own `.formatter.exs` to import those configurations. This is an example:

```elixir
# .formatter.exs
[
  import_deps: [
    :ash,
    :ash_postgres,
    :ash_json_api,
    :ash_graphql
  ],
  inputs: ["*.{ex,exs}", "priv/*/seeds.exs", "{config,lib,test}/**/*.{ex,exs}"],
  subdirectories: ["priv/*/migrations"]
]
```

There is not support for the automatic generation of a .formatter for _custom_ extensions, but if you're developing an extension library you can use the `mix ash.formatter` task to
automatically generate a formatter for your DSL. Eventually, we will want to add support for _adding_ to a .formatter.exs from custom extensions.
