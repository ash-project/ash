<!--
SPDX-FileCopyrightText: 2022 spark contributors <https://github.com/ash-project/spark/graphs.contributors>

SPDX-License-Identifier: MIT
-->

# Setting up autocomplete

## Compatibility

Autocomplete is enhanced by a plugin to ElixirSense, and therefore it only works for those who are using [ElixirLS](https://github.com/elixir-lsp/elixir-ls). We may consider adding the same extension to other language servers in the future.

## Setting it up

### DSL Modules

Inside of DSL modules, there is nothing you need to do! Autocomplete "just works" because ElixirSense finds the extension present inside of the Spark dependency.

### Options to functions using `Spark.Options`

To get autocomplete with documentation for the options to your functions, you need to add an `@doc` metadata that contains the index of the argument that this applies to, and the schema. Here is a complete example:

```elixir
@schema [
  verbose?: [
    type: :boolean,
    doc: "Whether or not to log verbose messages to the console",
    default: false
  ]
]

@doc spark_opts: [{1, @schema}]
def do_something(arg, opts \\ []) do
  opts = Spark.Options.validate!(opts, @schema)

  ...
end
```
