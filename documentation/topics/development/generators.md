<!--
SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs.contributors>

SPDX-License-Identifier: MIT
-->

# Generators

Ash comes with multiple generators, packaged as mix tasks, to help you generate and make modifications to your applications.

See the documentation for each mix task for more information. What is presented here is merely an overview.

## Installer

Ash can be installed into a project using `igniter`. Some examples of how this can work:

- Install Ash & AshPostgres into your current project

  ```
  mix igniter.install ash ash_postgres
  ```

- Create a new mix project with Ash & AshPostgres installed

  ```
  mix archive.install hex igniter_new
  mix igniter.new my_project --install ash,ash_postgres
  ```

- Create a new phoenix project with Ash & AshPostgres installed

  ```
  mix igniter.new my_project --install ash,ash_postgres,ash_phoenix --with phx.new
  ```

  > ### install hex archives {: .info}
  >
  > The archives have to be installed to use them. This only needs to be done once, until you change elixir versions.
  >
  > ```elixir
  > mix archive.install hex igniter_new
  > mix archive.install hex phx_new
  > ```

## Generators

- `mix ash.gen.resource` - Generates a new `Ash.Resource`.
- `mix ash.gen.domain` - Generates a new `Ash.Domain`.
- `mix ash.gen.enum` - Generates a new `Ash.Type.Enum`.
- `mix ash.gen.base_resource` - Generates a new base resource.
- `mix ash.gen.change` - Generates a new `Ash.Resource.Change`.
- `mix ash.gen.validation` - Generates a new `Ash.Resource.Validation`.
- `mix ash.gen.preparation` - Generates a new `Ash.Resource.Preparation`.
- `mix ash.gen.custom_expression` - Generates a new `Ash.CustomExpression`.

## Patchers

- `mix ash.extend` - Adds an extension or extensions to a domain or resource.
