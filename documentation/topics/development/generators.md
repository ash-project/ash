# Generators

Ash comes with multiple generators, packages as mix tasks, to help you generate and make modifications to your applications.

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
  mix igniter.new my_project --install ash,ash_postgres --with phx.new
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

- `mix ash.gen.resource` - Generates a new `Ash.Resource`
- `mix ash.gen.domain` - Generates a new `Ash.Domain`
- `mix ash.gen.enum` - Generates a new `Ash.Type.Enum`
- `mix ash.gen.base_resource` - Generates a new base resource.

## Patchers

- `mix ash.patch.extend` - Adds an extension or extensions to a domain or resource.
