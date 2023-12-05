# Auto-formatting Ash code

Ash comes with several utilities that can help keep your modules consistently formatted and organized.

## Basic setup

Add `:ash` (and any other Ash libraries you are using) to your `.formatter.exs` file:

```elixir
[
  # ...
  import_deps: [..., :ash],
  # ...
]
```

This means that when you autoformat your code, either via `mix format` or via an integration in your code editor, the exported data from Ash's `.formatter.exs` will be included and followed.

It includes definitions for `locals_without_parens`, meaning that your DSL builder code such as `attribute :name, :string` won't have parentheses added (to make it `attribute(:name, :string)`) when formatting the file. The parentheses won't be removed if they currently exist, but they won't be added if missing when formatting.

## Spark.Formatter

For more granular formatting, you can use `Spark.Formatter`, from the `spark` library.

> #### What is `spark`? {: .info}
>
> [`spark`](https://hexdocs.pm/spark) is a small library for building domain-specific languages (DSLs), and is what Ash itself uses internally. It provides the secret sauce to allow you to write your resources declaratively, and the editor integration so you get full autocomplete and documentation for free.

Add `Spark.Formatter` as a plugin in your `.formatter.exs` file:

```elixir
[
  # ...
  plugins: [..., Spark.Formatter]
  # ...
]
```

By itself, `Spark.Formatter` doesn't do much - but it is configurable.

The most common configuration is to remove _all_ extra parentheses from your DSL builder code - this is how the examples within Ash documentation is formatted.

To enable this, add the following line to your application config in `config.exs`:

```elixir
config :spark, :formatter, remove_parens?: true, "Ash.Resource": []
```

This tells Spark that it should remove parenthesis from all modules that use `Ash.Resource`, following the `locals_without_parens` rules exported from all your dependencies (and any you may have added yourself).

`Spark.Formatter` has more configuration available - check the documentation for more details!
