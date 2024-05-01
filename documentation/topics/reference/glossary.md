# Glossary

## Action

An action describes an operation that can be performed for a given resource; it is the _verb_ to a resource's _noun_. Examples of actions:

- User._create_
- Comment._delete_
- BlogPost._publish_
- Article._search_

Ash supports five different types of actions. `create`, `read`, `update` and `destroy` (collectively often abbreviated as CRUD), and `action`, referring to a generic action with a custom return type. A resource can define multiple actions per action type, eg. a _publish_ action would be considered an `update` because it is updating an existing instance of a resource. Actions are much more flexible than simple CRUD, but these five action types serve as templates for anything you might want to do.

See the [Actions guide](/documentation/topics/actions/actions.md) for more.

## Actor

The entity that performs an action. Most actions are run on direct user request, eg. if a user presses a Create button on a page then the actor is the user; but an actor might also be an organization, a group, or even a device.

Actors can be used in a number of places, from modifying the behavior of an action to auditing who did what in your system. They are most prominent, however, when writing [policies](#policy).

See the [Actors & Authorization guide](/documentation/topics/security/actors-and-authorization.md) for more.

## Aggregate

An aggregate is a special type of field for a resource, one that summarizes related information of the record. A more specialized type of a [calculation](#calculation).

If a Project resource has_many Ticket resources, an example of an aggregate on the Project might be to count the tickets associated to each project.

See the [Aggregates guide](/documentation/topics/resources/aggregates.md) for more.

## Attribute

A piece of data belonging to a resource. The most basic building block; an attribute has a type and a value. For resources backed by a data layer, they typically represent a column in a database table, or a key in an object store, for example.

See the [Attributes guide](/documentation/topics/resources/attributes.md) for more.

## Authorizer

An authorizer is an extension that can be added to a resource that will be given the opportunity to modify and/or prevent requests to a resource. In practice, you will almost always be using `Ash.Policy.Authorizer`, but you can still write your own if you need to.

See the [Actors & Authorization](documentation/topics/security/actors-and-authorization.md) and [Policies](documentation/topics/security/policies.md) guides for more.

## Calculation

A calculation is a special type of field for a resource, one that is not directly stored in the data layer but generated on-demand. Typically it will derive from other information on the record, but it may come from some other data source entirely.

See the [Calculations guide](/documentation/topics/resources/calculations.md) for more.

## Changeset

Changesets encapsulate data changes made while creating or updating an instance of a resource. Similarly to [Ecto changesets](https://hexdocs.pm/ecto/Ecto.Changeset.html), they include data validations but they also have their own callback hook lifecycle.

See `Ash.Changeset` for more.

## Domain

A method of broadly separating resources into different [domains](<https://en.wikipedia.org/wiki/Domain_(software_engineering)>), A.K.A [bounded contexts](https://martinfowler.com/bliki/BoundedContext.html).

See the [Domains guide](/documentation/topics/domains.md) for more.

## Extension

A packaged bundle of code that can be included in a resource to provide additional functionality. Built-in functionality such as the resource DSL itself is provided by an extension, and libraries like AshPostgres and AshAdmin also provide extensions that you can add to your resources with just one line of code.

See [Extending Resources](/documentation/topics/advanced/writing-extensions.md) for more.

## Filter

Filters are applied to queries to limit the data returned. They can also be applied to changesets, to ensure only data matching a certain condition is updated. For example:

- Fetching Articles that include a certain search term in the title
- Fetching Posts created by a specific user
- Fetching Tickets updated in the last week
- Updating a record only if it's version matches your in memory version (optimistic locking)

See `Ash.Filter` for more.

## Identity

A way to uniquely identify an instance of a resource. A primary key is an example of an identity that is automatically generated; you can manually add others such as a user's email address, or a URL slug for a post. If using AshPostgres, constraints will be created by the migration generator to enforce identities at the database level.

See the [Identities guide](/documentation/topics/resources/identities.md) for more.

## Notifier

Notifiers are modules that are called for each action that occurs on a resource (except generic actions). They are called at the end of transactions, meaning that if a notifier is called, it is guaranteed that the action they pertain to has completed successfully.

See the [Notifiers guide](/documentation/topics/resources/notifiers.md) for more.

## Policy

A set of rules defining who is authorized to perform specific actions on a resource. Common policy checks include rules such as:

- Forbidding anyone other than the user who wrote a blog post, from editing it
- Allowing only admins to update site-wide settings

See the [Policies guide](/documentation/topics/security/policies.md) for more.

## Query

The tools and functions used for reading and filtering stored data, from the data layer.

See `Ash.Query` for more.

## Record

A record is an "instance" of a resource. Keep in mind that this is not an "instance" in a mutable/object-oriented sense, but rather a snapshot of the data at a given point in time. When you call a read action, you get back records. You can provide a record to an update action to determine what data is being updated.

## Relationship

Relationships are named links between resources, that define how they relate to each other. Relationships can be used to signify ownership of a record, membership of a group, or can be used in filtering and querying data.

See the [Relationships guide](/documentation/topics/resources/relationships.md) for more.

## Resource

The central concept in Ash, a resource can be used to model all kinds of things. Most often, they will map to a data store, and represent things like rows of a database table. However, they can also be backed by external data sources, be used for validating data with no persistence at all, or even be simple containers for generic actions, completely stateless.

See the [Resource DSL docs](dsl-ash-resource.html) for DSL documentation.

## Tenant

Multitenancy is the siloing of your app's data into discrete non-overlapping groups, typically by customer or organization (the tenant). Ash supports multitenancy both at the code level and the data layer level (depending on the data layer; for example, AshPostgres uses schemas to fully separate data per tenant.)

See the [Multitenancy guide](/documentation/topics/advanced/multitenancy.md) for more.
