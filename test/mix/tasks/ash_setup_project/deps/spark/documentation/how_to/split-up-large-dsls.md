<!--
SPDX-FileCopyrightText: 2020 Zach Daniel
SPDX-FileCopyrightText: 2022 spark contributors <https://github.com/ash-project/spark/graphs.contributors>

SPDX-License-Identifier: MIT
-->

# Splitting Up Large DSLs

When building large DSLs, we face similar problems as things like large configuration files. It can be hard to find what we're looking for, and we can end up scrolling through a lot of DSL code to find what we're interested in. We generally suggest avoiding splitting up your DSLs by default, but it is important to know how to do so when the need arises.

## Fragments

Spark offers a tool called `Spark.Dsl.Fragment`, which allows you to compose a single DSL from multiple smaller DSL modules. There are a few important properties and caveats to understand:

1. Fragments are _not_ designed for sharing code between instances of a spark DSL. They are not dynamic. For creating behavior that extends across multiple instances of a DSL, you should write an extension.

2. A DSL has all extensions that any of its fragments has.

3. Fragments must express what they are a fragment _of_.

### Example

```elixir
defmodule MyApp.Accounts.User.Fragments.DataLayer do
  use Spark.Dsl.Fragment,
    of: Ash.Resource,
    data_layer: AshPostgres.DataLayer

  postgres do
    table "users"
    repo MyApp.Repo
    ...
  end
end

defmodule MyApp.Accounts.User do
  use Ash.Resource,
    fragments: [MyApp.Accounts.User.Fragments.DataLayer]

  ...
end
```
