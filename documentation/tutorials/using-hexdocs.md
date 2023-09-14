# Using Hexdocs

Ash is split across various packages. Each package has its own documentation. However, there is a global documentation search available at https://ash-hq.org. Do use it, use `Ctrl-K` or `Cmd-K` on that site.

## Packages

- [Ash](https://hexdocs.pm/ash): The core framework, providing all the features and goodies that power and enable the rest of the ecosystem.
- [AshPostgres](https://hexdocs.pm/ash_postgres): A PostgreSQL data layer for Ash resources, allowing for rich query capabilities and seamless persistence.
- [AshPhoenix](https://hexdocs.pm/ash_phoenix): Utilities for using Ash resources with Phoenix Framework, from building forms to running queries in sockets & LiveViews.
- [AshGraphql](https://hexdocs.pm/ash_graphql): A GraphQL extension that allows you to build a rich and customizable GraphQL API with minimal configuration required.
- [AshJsonApi](https://hexdocs.pm/ash_json_api): A JSON:API extension that allows you to effortlessly create a JSON:API spec compliant API.
- [AshAuthentication](https://hexdocs.pm/ash_authentication): Provides drop-in support for user authentication with various strategies and tons of customizability.
- [AshAuthenticationPhoenix](https://hexdocs.pm/ash_authentication_phoenix): Phoenix helpers and UI components in support of AshAuthentication.
- [AshStateMachine](https://hexdocs.pm/ash_state_machine): An Ash.Resource extension for building finite state machines.
- [AshCsv](https://hexdocs.pm/ash_csv): A CSV data layer allowing resources to be queried from and persisted in a CSV file.
- [AshDoubleEntry](https://hexdocs.pm/ash_double_entry): A customizable double entry bookkeeping system backed by Ash resources.
- [AshArchival](https://hexdocs.pm/ash_archival): A light-weight resource extension that modifies resources to simulate deletion by setting an `archived_at` attribute.
- [Reactor](https://hexdocs.pm/reactor): Reactor is a dynamic, concurrent, dependency resolving saga orchestrator.
- [Spark](https://hexdocs.pm/spark): The core DSL and extension tooling, allowing for powerful, extensible DSLs with minimal effort.



## DSL documentations
Some helpful tips on using Hex Docs. DSLs are each documented in their own area. Find them in the bottom of the sidebar on the left.

### Searching

#### In the sidebar

When searching for a dsl, prefix your search with `DSL: `. If you know the path
to the DSL you are looking for, use it separated by dots. For example, `DSL: attributes.attribute`. Only five results will show up in the sidebar, so be as specific as possible. If you don't find it, press enter and you will be taken to the search page.

#### In the search page

Searching by type will be supported soon, but has not yet been implemented. Once it is done, you will be able to search for `type:dsl` to filter for DSLs.
By default, search terms are considered optional. You can prefix them with + to make them required. Something you would do to find a specific DSL is to search for `+type:dsl +something`.
