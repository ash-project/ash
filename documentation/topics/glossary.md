# Glossary

## Action

An action describes an operation that can be performed for a given resource; it is the _verb_ to a resource's _noun_. Examples of actions:

- User._create_
- Comment._delete_
- BlogPost._publish_
- Article._search_

Ash supports four different types of actions - `create`, `read`, `update` and `destroy` (collectively often abbreviated as CRUD). A resource can define multiple actions per action type, eg. a _publish_ action would be considered an `update` because it is updating an existing instance of a resource. Actions are much more flexible than simple CRUD, but these four action types serve as templates for anything you might want to do.

See the [Actions guide](/documentation/topics/actions.md) for more information.

## Actor

The entity that performs an action. Most actions are run on direct user request, eg. if a user presses a Create button on a page then the actor is the user; but an actor might also be an organization, a group, or the system itself.

Actors are referenced during authorization - ensuring that the actor is allowed to perform an action, before it takes place. The actor can also be used within actions, to record which entity performed the action.

See the [Security guide](/documentation/topics/security.md#actors) for more information.

## Aggregate

An aggregate is a special type of field for a resource, one that summarizes related information of the record. A more specialized type of a [calculation](#calculation).

If a Project resource has_many Ticket resources, an example of an aggregate on the Project might be to count the tickets associated to each project.

See the [Aggregates guide](/documentation/topics/aggregates.md) for more information.

## API

A method of broadly separating resources into different [bounded contexts](https://martinfowler.com/bliki/BoundedContext.html). Small apps might only have one API, in which case you can set-and-forget it, but apps with larger domains can benefit from different contexts having different views of the same resource.

See `Ash.Api.Dsl` for more information.

## Attribute

A piece of data belonging to a resource. The most basic building block; an attribute has a type and a value and is stored within the context of a domain model.

See `d:Ash.Resource.Dsl.attributes` for more information.

## Calculation

A calculation is a special type of field for a resource, one that is not directly stored in the data layer but generated on-demand when specifically requested as part of a query. Typically it will derive from other information on the record, but it may come from some other data source entirely.

See the [Calculations guide](/documentation/topics/calculations.md) for more information.

## Changeset

Changesets encapsulate data changes made while creating or updating an instance of a resource. Similarly to [Ecto changesets](https://hexdocs.pm/ecto/Ecto.Changeset.html), they include data validations but they also have their own callback hook lifecycle.

See `Ash.Changeset` for more information.

## Extension

A packaged bundle of code that can be included in a resource to provide additional functionality. Built-in functionality such as the resource DSL itself is provided by an extension, and libraries like AshPostgres and AshAdmin also provide extensions that you can add to your resources with just one line of code.

See [Extending Resources](/documentation/tutorials/extending-resources.md) for more information.

## Filter

The tools and functions used to reduce the amount of data returned when running queries on the data layer. This may look like:

- Fetching Articles that include a certain search term in the title
- Fetching Posts created by a specific user
- Fetching Tickets updated in the last week

See `Ash.Filter` for more information.

## Flow

Flows combine actions together into a static workflow, somewhat similarly to [Ecto.Multi](https://hexdocs.pm/ecto/Ecto.Multi.html). The result of running one action in a flow can be used as input to another action, and flows can be halted and resumed on request. Flows also support transactions; steps can be grouped together into transactions or the whole flow can be run inside a single transaction.

See the [Flows guide](/documentation/topics/flows.md) for more information.

## Identity

A way to uniquely identify an instance of a resource. A primary key is an example of an identity that is automatically generated; you can manually add others such as a user's email address, or a URL slug for a post. If using AshPostgres, constraints will be created to enforce identities at the database level.

See the [Identities guide](/documentation/topics/identities.md) for more information.

## Notifier

Notifiers are modules that insert callbacks into the lifecycle of a resource action, to be _notified_ when actions take place. When connected to a resource they are invoked after every action, but via pattern matching on the notification received, they can be made very granular.

See the [Notifiers guide](/documentation/topics/notifiers.md) for more information.

## Policy

A set of rules defining who is authorized to perform specific actions on a resource. Common policy checks include rules such as:

- Forbidding anyone other than the user who wrote a blog post, from editing it
- Allowing only admins to update site-wide settings

See the [Policies guide](/documentation/topics/policies.md) for more information.

## Query

The tools and functions used for reading and filtering stored data, from the data layer.

See `Ash.Query` for more information.

## Relationship

Relationships (also known as _associations_) are named links between resources, that define how they relate to each other. Relationships can be used to signify ownership of a record, membership of a group, or can be used in filtering and querying data.

See the [Relationships guide](/documentation/topics/relationships.md) for more information.

## Registry

A registry defines the set of resources available in your application, via listing entries. You'll probably never need to interact with one directly after setting it up, but it works efficiently for compile-time optimization.

## Resource

The central concept in Ash, a resource is a domain model object in your system, the nouns that your app revolves around. Resources contain definitions of the data they hold in the form of attributes, but also define [actions](#action) that can be taken on that data and [actors](#actor) that are allowed to run them.

It is not a strict requirement that resources contain data - they can be used purely to create a standard interface for performing tasks - but in practice, most resources will be used to manage data.

See `Ash.Resource.Dsl` for more information.

## Tenant

Multitenancy is the siloing of your app's data into discrete non-overlapping groups, typically by customer or organization (the tenant). Ash supports multitenancy both at the code level and the data layer level (depending on the data layer; for example, AshPostgres uses schemas to fully separate data per tenant.)

See the [Multitenancy guide](/documentation/topics/multitenancy.md) for more information.
