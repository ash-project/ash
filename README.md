![Logo](https://github.com/ash-project/ash/blob/main/logos/cropped-for-header-black-text.png?raw=true#gh-light-mode-only)
![Logo](https://github.com/ash-project/ash/blob/main/logos/cropped-for-header-white-text.png?raw=true#gh-dark-mode-only)

![Elixir CI](https://github.com/ash-project/ash/workflows/Ash%20CI/badge.svg)
[![OpenSSF Scorecard](https://api.scorecard.dev/projects/github.com/ash-project/ash/badge)](https://scorecard.dev/viewer/?uri=github.com/ash-project/ash)
[![OpenSSF Best Practices](https://www.bestpractices.dev/projects/10686/badge)](https://www.bestpractices.dev/projects/10686)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
[![Hex version badge](https://img.shields.io/hexpm/v/ash.svg)](https://hex.pm/packages/ash)
[![Hexdocs badge](https://img.shields.io/badge/docs-hexdocs-purple)](https://hexdocs.pm/ash)

# Ash Framework

Welcome! Here you will find everything you need to know to get started with and use Ash. This documentation is best viewed on [hexdocs](https://hexdocs.pm/ash).

## Dive In

- [What is Ash?](documentation/topics/about_ash/what-is-ash.md)
- [Getting Started Tutorial](documentation/tutorials/get-started.md)
- [See the roadmap](https://github.com/orgs/ash-project/projects/3)

## About the Documentation

The [Get Started Livebook](documentation/tutorials/get-started.md) **Tutorial** introduces you to the core concepts of Ash like resources, actions, and relationships. It’s a great place to start if you are a beginner.

---

[**Topics**](#topics) provide a high level overview of a specific concept or feature of Ash. These are **understanding-oriented**, and are perfect for discovering design patterns, features, and tools.

---

[**How-to**](#how-to) guides are **goal-oriented** recipes for accomplishing specific popular tasks. Browse to get an idea of how Ash works and what is possible with it.

---

[**Reference**](#reference) documentation is **information-oriented**, covering every Ash module, function, expression, and DSL. It is produced automatically from our source code. Use the sidebar and the top search
bar to find relevant reference information. Place the text `dsl` before your search to quickly jump to a particular DSL — e.g. try comparing the results of searching for `name` with the results for `dsl name`.

---

## Topics

### About Ash

- [What is Ash?](documentation/topics/about_ash/what-is-ash.md)
- [Our Design Principles](documentation/topics/about_ash/design-principles.md)
- [Contributing to Ash](.github/CONTRIBUTING.md)
- [Alternatives](documentation/topics/about_ash/alternatives.md)

### Development

- [Project Structure](documentation/topics/development/project-structure.md)
- [Generators](documentation/topics/development/generators.md)
- [Testing](documentation/topics/development/testing.md)
- [Working with LLM Assistance](documentation/topics/development/working-with-llms.md)
- [Development Utilities](documentation/topics/development/development-utilities.md)
- [Backwards Compatibility Config](documentation/topics/development/backwards-compatibility-config.md)
- [Upgrading to 3.0](documentation/topics/development/upgrading-to-3.0.md)
- [Error Handling](documentation/topics/development/error-handling.md)

### Resources

- [Domains](documentation/topics/resources/domains.md)
- [Attributes](documentation/topics/resources/attributes.md)
- [Relationships](documentation/topics/resources/relationships.md)
- [Calculations](documentation/topics/resources/calculations.md)
- [Aggregates](documentation/topics/resources/aggregates.md)
- [Code Interfaces](documentation/topics/resources/code-interfaces.md)
- [Identities](documentation/topics/resources/identities.md)
- [Validations](documentation/topics/resources/validations.md)
- [Changes](documentation/topics/resources/changes.md)
- [Preparations](documentation/topics/resources/preparations.md)
- [Embedded Resources](documentation/topics/resources/embedded-resources.md)
- [Notifiers](documentation/topics/resources/notifiers.md)

### Actions

- [Actions](documentation/topics/actions/actions.md)
- [Read Actions](documentation/topics/actions/read-actions.md)
- [Create Actions](documentation/topics/actions/create-actions.md)
- [Update Actions](documentation/topics/actions/update-actions.md)
- [Destroy Actions](documentation/topics/actions/destroy-actions.md)
- [Generic Actions](documentation/topics/actions/generic-actions.md)
- [Manual Actions](documentation/topics/actions/manual-actions.md)

### Security

- [Actors & Authorization](documentation/topics/security/actors-and-authorization.md)
- [Sensitive Data](documentation/topics/security/sensitive-data.md)
- [Policies](documentation/topics/security/policies.md)

### Advanced

- [Multi-step Actions](documentation/topics/advanced/multi-step-actions.md)
- [Monitoring](documentation/topics/advanced/monitoring.md)
- [Multitenancy](documentation/topics/advanced/multitenancy.md)
- [Reactor](documentation/topics/advanced/reactor.md)
- [Combination Queries](documentation/topics/advanced/combination-queries.md)
- [Timeouts](documentation/topics/advanced/timeouts.md)
- [Writing Extensions](documentation/topics/advanced/writing-extensions.md)

---

## How-to

- [Write Queries](documentation/how-to/write-queries.livemd)
- [Test Resources](documentation/how-to/test-resources.livemd)
- [Authorize Access to Resources](documentation/how-to/authorize-access-to-resources.livemd)
- [Encrypt Attributes](documentation/how-to/encrypt-attributes.livemd)
- [Prevent Concurrent Writes](documentation/how-to/prevent-concurrent-writes.livemd)
- [Wrap External APIs](documentation/how-to/wrap-external-apis.livemd)
- [Define Polymorphic Relationships](documentation/how-to/polymorphic-relationships.livemd)

---

## Reference

- [Ash.Resource DSL](documentation/dsls/DSL-Ash.Resource.md)
- [Ash.Domain DSL](documentation/dsls/DSL-Ash.Domain.md)
- [Ash.Reactor DSL](documentation/dsls/DSL-Ash.Reactor.md)
- [Ash.Notifier.PubSub DSL](documentation/dsls/DSL-Ash.Notifier.PubSub.md)
- [Ash.Policy.Authorizer DSL](documentation/dsls/DSL-Ash.Policy.Authorizer.md)
- [Ash.DataLayer.Ets DSL](documentation/dsls/DSL-Ash.DataLayer.Ets.md)
- [Ash.DataLayer.Mnesia DSL](documentation/dsls/DSL-Ash.DataLayer.Mnesia.md)
- [Glossary](documentation/topics/reference/glossary.md)
- [Expressions](documentation/topics/reference/expressions.md)
- For other reference documentation, see the sidebar & search bar

## Packages

The Ash ecosystem consists of numerous packages, all of which have their own documentation. If you can't find something in this documentation, don't forget to search in any potentially relevant package.

### Data Layers

- [AshPostgres](https://hexdocs.pm/ash_postgres) | PostgreSQL data layer
- [AshSqlite](https://hexdocs.pm/ash_sqlite) | SQLite data layer
- [AshCsv](https://hexdocs.pm/ash_csv) | CSV data layer
- [AshCubdb](https://hexdocs.pm/ash_cubdb) | CubDB data layer

### API Extensions

- [AshJsonApi](https://hexdocs.pm/ash_json_api) | JSON:API builder
- [AshGraphql](https://hexdocs.pm/ash_graphql) | GraphQL builder

### AI

- [AshAI](https://hexdocs.pm/ash_ai) | Structured Outpus, MCP, Vectorization and more

### Web

- [AshPhoenix](https://hexdocs.pm/ash_phoenix) | Phoenix integrations
- [AshAuthentication](https://hexdocs.pm/ash_authentication) | Authenticate users with password, OAuth, and more
- [AshAuthenticationPhoenix](https://hexdocs.pm/ash_authentication_phoenix) | Integrations for AshAuthentication and Phoenix

### Finance

- [AshMoney](https://hexdocs.pm/ash_money) | A money data type for Ash
- [AshDoubleEntry](https://hexdocs.pm/ash_double_entry) | A double entry system backed by Ash Resources

### Resource Utilities

- [AshOban](https://hexdocs.pm/ash_oban) | Background jobs and scheduled jobs for Ash, backed by Oban
- [AshArchival](https://hexdocs.pm/ash_archival) | Archive resources instead of deleting them
- [AshStateMachine](https://hexdocs.pm/ash_state_machine) | Create state machines for resources
- [AshPaperTrail](https://hexdocs.pm/ash_paper_trail) | Keep a history of changes to resources
- [AshCloak](https://hexdocs.pm/ash_cloak) | Encrypt attributes of a resource

### Admin & Monitoring

- [AshAdmin](https://hexdocs.pm/ash_admin) | A push-button admin interface
- [AshAppsignal](https://hexdocs.pm/ash_appsignal) | Monitor your Ash resources with AppSignal

### Testing

- [Smokestack](https://hexdocs.pm/smokestack) | Declarative test factories for Ash resources

### Examples & External Resources

- [Ash Real World](https://github.com/team-alembic/realworld) | Ash + Phoenix LiveView codebase containing real world examples (CRUD, auth, advanced patterns, etc)
