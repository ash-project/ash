<!--
SPDX-FileCopyrightText: 2020 Zach Daniel
SPDX-FileCopyrightText: 2022 spark contributors <https://github.com/ash-project/spark/graphs.contributors>

SPDX-License-Identifier: MIT
-->

# Writing Extensions

Writing extensions generally involves three main components.

## The DSL declaration

The DSL is declared as a series of `Spark.Dsl.Section`, which can contain `Spark.Dsl.Entity` and further `Spark.Dsl.Section` structs. See `Spark.Dsl.Section` and `Spark.Dsl.Entity` for more information.

## Transformers

Extension writing gets a bit more complicated when you get into the world of transformers, but this is also where a lot of the power is. Each transformer can declare other transformers it must go before or after, and then is given the opportunity to modify the entirety of the DSL it is extending up to that point. This allows extensions to make rich modifications to the structure in question. See `Spark.Dsl.Transformer` for more information

## Introspection

Use functions in `Spark.Dsl.Extension` to retrieve the stored values from the DSL and expose them in a module. The convention is to place functions for something like `MyApp.MyExtension` in `MyApp.MyExtension.Info`. Using introspection functions like this allows for a richer introspection API (i.e not just getting and retrieving raw values), and it also allows us to add type specs and documentation, which is helpful when working generically. I.e `module_as_variable.table()` can't be known by dialyzer, whereas `Extension.table(module)` can be.

## Source Annotations

Spark automatically tracks source location information for all DSL elements. This enables better error messages, IDE integration, and debugging capabilities. See [Using Source Annotations](use-source-annotations.md) for details on accessing and using this information in your extensions.
