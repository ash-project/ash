![Test Image 6](https://github.com/ash-project/ash/blob/master/logos/cropped-for-header.png)

![Elixir CI](https://github.com/ash-project/ash/workflows/Elixir%20CI/badge.svg)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
[![Coverage Status](https://coveralls.io/repos/github/ash-project/ash/badge.svg?branch=master)](https://coveralls.io/github/ash-project/ash?branch=master)
[![Hex version badge](https://img.shields.io/hexpm/v/ash.svg)](https://hex.pm/packages/ash)

## Quick Links

- [Resource Documentation](https://hexdocs.pm/ash/Ash.Resource.html)
- [DSL Documentation](https://hexdocs.pm/ash/Ash.Resource.DSL.html)
- [Code API documentation](https://hexdocs.pm/ash/Ash.Api.Interface.html)

## Introduction

Traditional MVC Frameworks (Rails, Django, .Net, Phoenix, etc) leave it up to the user to build the glue between requests for data (HTTP requests in various forms as well as server-side domain logic) and their respective ORMs. In that space, there is an incredible amount of boilerplate code that must get written from scratch for each application (authentication, authorization, sorting, filtering, sideloading relationships, serialization, etc).

Ash is an opinionated yet configurable framework designed to reduce boilerplate in an Elixir application. Ash does this by providing a layer of abstraction over your system's data layer(s) with `Resources`. It is designed to be used in conjunction with a phoenix application, or on its own.

To riff on a famous JRR Tolkien quote, a `Resource`is "One Interface to rule them all, One Interface to find them" and will become an indispensable place to define contracts for interacting with data throughout your application.

To start using Ash, first declare your `Resources` using the Ash `Resource` DSL. You could technically stop there, and just leverage the Ash Elixir API to avoid writing boilerplate. More likely, you would use extensions like Ash.JsonApi or Ash.GraphQL with Phoenix to add external interfaces to those resources without having to write any extra code at all.

Ash is an open-source project and draws inspiration from similar ideas in other frameworks and concepts. The goal of Ash is to lower the barrier to adopting and using Elixir and Phoenix, and in doing so help these amazing communities attract new developers, projects, and companies.

## Example Resource

```elixir
defmodule Post do
  use Ash.Resource
  use AshJsonApi.JsonApiResource
  use Ash.DataLayer.Postgres

  actions do
    read :default

    create :default
  end

  attributes do
    attribute :name, :string
  end

  relationships do
    belongs_to :author, Author
  end
end
```

## TODO LIST (in no order)

Filters

This is from a conversation about how we might support on-demand calculated attributes
that would allow doing something like getting the trigram similarity to a piece of text
and filtering on it in the same request

```elixir
  :representatives
  |> Api.query()
  |> Ash.Query.filter(first_name: [trigram: [text: "Geoff", similarity: [greater_than: 0.1]]])
  |> Api.read!()

  calculated_attributes do
    on_demand :trigram, [:first_name, :last_name]
    # calculated(:name_similarity, :float, calculate: [trigram_similarity: {:input, :text}])
  end

  :representatives
  |> Api.query()
  |> Ash.Query.calculate(first_name_similarity: [first_name: [trigram: [text: "Geoff"]]])
  |> Ash.Query.filter(first_name_similarity: [greater_than: 0.1])
  |> Api.read!()
```

Actions

- all actions need to be performed in a transaction
- Since actions can return multiple errors, we need a testing utility to unwrap/assert on them
- Validate that checks have the correct action type when compiling an action
- Handle related values on delete
- If structs/a struct is passed into a relationship change like `api.update(:author, relationships: %{posts: %Post{}})` we shouldn't have to read it back
- Support bulk creation - use this when managing to many relationships if available
- support deleting w/ a query - use this when managing to many relationships
- Support fields on join tables if they are a resource
- dont require a join table for many_to_many relationships

Authorization

- document authorization thoroughly. _batch_ (default) checks need to return a list of `ids` for which the check passed.
- Do branch analysis of each record after authorizing it, in authorizer
- Make authorization spit out informative errors (at least for developers)
- Rearchitect relationship updates so that they can be sensible authorized. As in, which resource is responsible for authorizing updates to a relationship? Should there be some unified way to describe it? Or is updating a user's posts an entirely separate operation from updating a post's user?
- Test authorization
- Support branching/more complicated control flow in authorization steps
- The Authorization flow for creates/updates may be insufficient. Instead of adding requests if relationships/attributes are changing, we may instead want to embed that knowledge inside the sat solver itself. Basically a `relationship_foo_is_changing` fact, *and*ed with the resulting conditions. I'm not even sure if thats possible though.
- Consider that authorization steps may eventually need to be able to branch. This may or may not be difficult :)
- Allow a feature called `verify_after_write` that fetches the final attribute variable from data somehow. Authorization fetchers will need to take state as an argument or something like that, and maybe need to specify dependencies?.
- Side load authorization testing
- Right now, includes will be generating their authorization facts independently. This is no great loss because anything that isn't data dependent should be a strict check anyway. However, we may at some point want to check to see if any filter exactly matches a side load filter, and authorize them together/have them share.
- Forbid impossible auth/creation situations (e.g "the id field is not exposed on a create action, and doesn't have a default, therefore writes will always fail.)
- perhaps have auth steps express which fields need to be present, so we can avoid loading things unnecessarily
- check if preparations have been done on a superset filter of a request and, if so, use it
- just had a cool thought: we can actually run the satsolver _before_ fetching the user. The satsolver could warn us early that _no user_ could make the request in question!
- Use the sat solver at compile time to tell people when requests they've configured (and maybe all combinations of includes they've allowed?) couldn't possibly be allowed together.
- optimize authorization by allowing facts to be shared amongst requests?
- handle errors from runtime filtering based on authorizer responses

Community

- [twirp](https://github.com/keathley/twirp)

CI

- Add support for more elixir/erlang versions, add them to the CI matrix

Relationships

- Validate that all relationships on all resources in the API have destinations _in_ that API, or don't and add in logic to pretend those don't exist through the API.
- validate reverse relationships!!
- document reverse relationships, or perhaps consider that we need a `from_relationship` filter instead?
- Factor out shared relationship options into its own schema, and merge them, for clearer docs.
- Naturally, only inner joins are allowed now. I think only inner joins will be necessary, as the pattern in ash would be to side load related data.
- Support arbitrary "through" relationships
- right now we don't support having duplicates in `many_to_many` relationships, so we'll need to document the limits around that: the primary key of the join resource must be (at least as it is in ash) the join keys. If they don't want to do that, then they should at a minimum define a unique constraint on the two join keys.
- relationship changes are an artifact of the old way of doing things and are very ugly right now
- Figure out under what circumstances we can bulk fetch when reading before updating many_to_many and to_many relationships, and do so.
- Perhaps, reverse relationships should eliminate the need to set destination field.
- also, perhaps, reverse relationships suck and should not be necessary. The alternative is a `from_related` filter, that lets you use an association in reverse
- allow configuring the name of the join relationship name for many to many relationships

Primary Key

- So many parts of the system are reliant on things having an `id` key explicitly. THis will need to be addressed some day, and will be a huge pain in the ass
- Raise on composite primary key if data layer can't do it

Params

- `params` should be solidified. Perhaps as a struct. Or perhaps just renamed to `action_params` where it is used.
- Consider making a "params builder" so you can say things like `Ash.Params.add_side_load(params, [:foo, :bar, :baz])` and build params up over time.
- Validate that params on the way in are either all strings or all atoms

Ecto

- The ecto internals that live on structs are going to cause problems w/ pluggability of backends, like the `%Ecto.Association.NotLoaded{}`. That backend may need to scrub the ecto specifics off of those structs.

ETS

- make ets dep optional
- Unit test the Ets data layer

Observability

- Use telemetry and/or some kind of hook system to add metrics

Data Layer

- BIG: support transactions, both detecting that we are in a transaction
  and specifying a cross data-layer transaction
- Allow encoding database-level constraints into the resource, like "nullable: false" or something. This will let us validate things like not leaving orphans when bulk updating a many to many
- Eventually data_layers should state what raw types they support, and the filters they support on those raw types
- Think hard about the data_layer.can? pattern to make sure we're giving enough info, but not too much.
- Use data layer compatibility features to disallow incompatible setups. For instance, if the data layer can't transact, then they can't have an editable `has_one` or `many_to_many` resource.
- Add `can?(:bulk_update)` to data layers, so we can more efficiently update relationships
- Set up "atomic updates" (upserts). If an adapter supports them, and the auth passes precheck, we could turn `get + update` combos into `upserts`
- Make an automatic test suite that confirms that data layers behave the way they claim to behave, maybe.
- Is it possible/reasonable to do join tables that aren't unique on source_id/destination_id? Probably, but metadata would need to be organized differently.
- right now, we require the datalayer to support upserts in order to do relationship additions, but we could set it up such that if the datalayer doesn't support upserts, we first read the relationship. Its not airtight against race conditions (unless the datalayer supports transactions, also unimplemented) but its better than nothing.
- without transactions, we can't ensure that all changes are rolled back in the case that relationship updates are included. Don't think there is really anything to do about that, but something worth considering.

DSL

- use a process to hold constructed DSL state, and then coalesce it all at the end. This can clean things up, and also allow us to potentially eliminate the registry. This will probably go hand in hand w/ the "capabilities" layer, where the DSL confirms that your data layer is capable of performing everything that your DSL declares
- Bake in descriptions to the DSL
- need to make sure that all of the dsl components are in the `.formatter.exs`. I made it so all of the options can be specified as a nested DSL, but haven't gone through and added them to the formatter file
- add `init` to checks, and error check their construction when building the DSL
- allow for resources to be created _without_ the dsl

Code Quality

- Replace all my ugly `reduce` with tuples with `reduce_while`

Runtime

- Add a runtime-intialization to checks that can return data loading instructions to be executed prior to pre-check
- Important: We need a framework level solution for runtime configuration, _or at minimum_ a recommended way to do it. Things like configuring the host/port of your API, or disabling features

Testing

- talk about building factory/mocking utilities. I suspect mocking will not be necessary due to eventually being able to toggle resources to use the ETS datalayer.

Engine

- implement transactions in the engine. Perhaps by assigning requests transaction ids or something along those lines.
- fix the comment noted in the destroy action: ~the delete needs to happen _outside_ of the data fetching step, so the engine needs some kind of "after data resolved" capability~
- add a total failure mode (since JSON API just fails entirely) that will cause the engine to stop on the first error
- Get rid of all of the :lists.droplast and List.first stuff by making a `%Ash.Engine.Dependecy{path: path, field: field}`
- consider (some day) opening up the engine/request as a public API

Fields/Attributes

- Flesh out field options (sortable, filterable, other behavior?)
- Booleans need to not support `nil` values. That has to be a different type. boolean filter/authorization logic is greatly enhanced if that is the case.
- Add the ability to configure what fields can identify a user (for instance email) and use those in checks.
- certain attribute names are not going to be allowed, like `or`, `and`, `in`, things like that.
- Make sure updating foreign key attributes behaves the same as setting a relationship, or just disallow having editable attributes for relationship fkeys

Framework

- support accepting a _resource and a filter_ in `api.update` and `api.destroy`, and doing those as bulk actions
- support something similar to the older interface we had with ash, like `Api.read(resource, filter: [...], sort: [...])`, as the `Ash.Query` method is a bit long form in some cases
- Add a mixin compatibility checker framework, to allow for extensions to declare what features they do/don't support.
  - Have ecto types ask the data layer about the kinds of filtering they can do, and that kind of thing.
- Make an `Ash.Changeset` that is a superset of an ecto changeset
- consider, just for the sake of good old fashion fun/cool factor, a parser that can parse a string into a query at compile time, so that queries can look nice in code.
- Make `Ash.Type` that is a superset of things like `Ecto.Type`. If we bring in ecto database-less(looking like more and more of a good idea to me) that kind of thing gets easier and we can potentially lean on ecto for type validations well.
- When we support embedding, figure out `embed_as` on `Ash.Type`
- Consider supporting one resource being a "more specific" version of another resource at the _resource_ level, not the data layer level.

## Creating a new release of Ash

- check out the repository locally
- run `mix git_ops.release` (see git_ops documentation for more information)
- check the changelog/new release number
- push (with tags) and CI will automatically deploy the hex package
