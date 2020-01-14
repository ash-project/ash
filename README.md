# Ash

## Quick Links
* [Resource Documentation](https://hexdocs.pm/ash/Ash.Resource.html)
* [DSL Documentation](https://hexdocs.pm/ash/Ash.Resource.DSL.html)
* [Code API documentation](https://hexdocs.pm/ash/Ash.Api.Interface.html)

## Introduction

Traditional MVC Frameworks (Rails, Django, .Net, Phoenix, etc) leave it up to the user to build the glue between requests for data (HTTP requests in various forms as well as server-side domain logic) and their respective ORMs. In that space, there is an incredible amount of boilerplate code that must get written from scratch for each application (authentication, authorization, sorting, filtering, pagination, sideloading relationships, serialization, etc).

Ash is an opinionated yet configurable framework designed to reduce boilerplate in Elixir application. Don't worry Phoenix developers - Ash is designed to play well with Phoenix too :). Ash does this by providing a layer of abstraction over your system's data layer(s) with `Resources`.

To riff on a famous JRR Tolkien quote, a `Resource`is "One Interface to rule them all, One Interface to find them" and will become an indispensable place to define contracts for interacting with data throughout your application.

To start using Ash, first declare your `Resources` using the Ash `Resource` DSL. You could technically stop there, and just leverage the Ash Elixir API to avoid writing boilerplate. More likely, you would use libraries like Ash.JsonApi or Ash.GraphQL(someday) with Phoenix to add external interfaces to those resources without having to write any extra code at all.

Developers should be focusing on their core business logic - not boilerplate code. Ash builds upon the incredible productivity of Phoenix and empowers developers to get up and running with a fully functional app in substantially less time, while still being flexible enough to allow customization when the need inevitably arises.

Ash is an open-source project and draws inspiration from similar ideas in other frameworks and concepts. The goal of Ash is to lower the barrier to adopting and using Elixir and Phoenix, and in doing so help these amazing communities attract new developers, projects, and companies.

## Example Resource
```elixir
defmodule Post do
  use Ash.Resource, name: "posts", type: "post"
  use AshJsonApi.JsonApiResource
  use Ash.DataLayer.Postgres

  actions do
    read :default,
      rules: [
        authorize_if: user_is(:admin)
      ]

    create :default,
      rules: [
        authorize_if: user_is(:admin)
      ]
    
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

* Make our router cabaple of describing its routes in `mix phx.routes` Chris McCord says that we could probably power that, seeing as phoenix controls both APIs, and that capability could be added to `Plug.Router`
* Finish the serializer
* DSL level validations! Things like includes validating that their chain exists. All DSL structs should be strictly validated when they are created.
* Especially at compile time, we should *never* ignore or skip invalid options. If an option is present and invalid, an error is raised.
* break up the `Ash` module
* Wire up/formalize the error handling (this is high priority)
* Ensure that errors are properly propagated up from the data_layer behaviour, and every operation is allowed to fail
* figure out the ecto schema warning
* all actions need to be performed in a transaction
* document authorization thoroughly. *batch* (default) checks need to return a list of `ids` for which the check passed.
* So many parts of the system are reliant on things having an `id` key explicitly. THis will need to be addressed some day, and will be a huge pain in the ass
* Validate that the user resource has a get action
* `params` should be solidified. Perhaps as a struct. Or perhaps just renamed to `action_params` where it is used.
* Since actions contain rules now, consider making it possible to list each action as its own `do` block, with an internal DSL for configuring the action. (overkill?)
* Validate rules at creation
* Maybe fix the crappy parts of optimal and bring it in for opts validation?
* The ecto internals that live on structs are going to cause problems w/ pluggability of backends, like the `%Ecto.Association.NotLoaded{}`. That backend may need to scrub the ecto specifics off of those structs.
* Add a mixin compatibility checker framework, to allow for mix_ins to declare what features they do/don't support.
  * Have ecto types ask the data layer about the kinds of filtering they can do, and that kind of thing.
* Make `Ash.Type` that is a superset of things like `Ecto.Type`. If we bring in ecto database-less(looking like more and more of a good idea to me) that kind of thing gets easier and we can potentially lean on ecto for type validations well.
* use a process to hold constructed DSL state, and then coalesce it all at the end. This can clean things up, and also allow us to potentially eliminate the registry. This will probably go hand in hand w/ the "capabilities" layer, where the DSL confirms that your data layer is capable of performing everything that your DSL declares
* make ets dep optional
* Bake in descriptions to the DSL
* Contributor guideline and code of conduct
* Do branch analysis of each record after authorizing it, in authorizer
* consider moving `type` and `name` for resources out into json api (or perhaps just `name`) since only json api uses that
* When we support embedding, figure out `embed_as` on `Ash.Type`
* Consider allowing declaring a data layer at the *api* level, or overriding the resource's data layer at the *api* level
* Since actions can return multiple errors, we need a testing utility to unwrap/assert on them
* Flesh out relationship options
* Flesh out field options (sortable, filterable, other behavior?)
* Unit test the Ets data layer
* Improve pagination in the ETS data layer
* Rearchitect relationship updates so that they can be sensible authorized. As in, which resource is responsible for authorizing updates to a relationship? Should there be some unified way to describe it? Or is updating a user's posts an entirely separate operation from updating a post's user?
* Test authorization
* Validate that all relationships on all resources in the API have destinations *in* that API, or don't and add in logic to pretend those don't exist through the API.
* Make authorization spit out informative errors (at least for developers)
* Use telemetry and/or some kind of hook system to add metrics
* Forbid impossible auth/creation situations (e.g "the id field is not exposed on a create action, and doesn't have a default, therefore writes will always fail.)
* Don't let users declare `has_one` relationships without claiming that there is a unique constraint on the destination field.
* Set up "atomic updates" (upserts). If an adapter supports them, and the auth passes precheck, we could turn `get + update` combos into `upserts` 
* Use data layer compatibility features to disallow incompatible setups. For instance, if the data layer can't transact, then they can't have an editable `has_one` or `many_to_many` resource.
* Add `can?(:bulk_update)` to data layers, so we can more efficiently update relationships
* Figure out under what circumstances we can bulk fetch when reading before updating many_to_many and to_many relationships, and do so.
* most relationship stuff can't be done w/o primary keys
* includer errors are super obscure because you can't tell what action they are about
* Allow encoding database-level constraints into the resource, like "nullable: false" or something. This will let us validate things like not leaving orphans when bulk updating a many to many
* Validate filters, now that there can be duplicates. Doesn't make sense to provide two "exact equals" filters
* Eventually data_layers should state what raw types they support, and the filters they support on those raw types
* Raise on composite primary key if data layer can't do it
* Add impossibility checking for filters to avoid running queries that will never be possible.
* As soon as we add array types, the filter logic is going to break because we use "is a list" as a criterion for "has not been passed a raw value to match". This may not be too big of a problem if we just don't support a list. But using some sort of actual struct to represent "this is constructed filter" may be the real answer.
* Add a runtime-intialization to checks that can return data loading instructions to be executed prior to pre-check
* Naturally, only inner joins are allowed now. I think only inner joins will be necessary, as the pattern in ash would be to side load related data.
* certain attribute names are not going to be allowed, like `or`, `and`, `in`, things like that.
* consider, just for the sake of good old fashion fun/cool factor, a parser that can parse a string into a query at compile time, so that queries can look nice in code.
* validate reverse relationships!!
* Factor out shared relationship options into its own schema, and merge them, for clearer docs.
* Consider making a "params builder" so you can say things like `Ash.Params.add_side_load(params, [:foo, :bar, :baz])` and build params up over time.
* validate using composite primary keys using the `data_layer.can?(:composite_primary_key)`
* Think hard about the data_layer.can? pattern to make sure we're giving enough info, but not too much.
* Use the sat solver at compile time to tell people when requests they've configured (and maybe all combinations of includes they've allowed?) couldn't possibly be allowed together.
* Support arbitrary "through" relationships
* Replace all my ugly `reduce` with tuples with `reduce_while`
* Framework internals need to stop using `api.foo`, because the code interface
  is supposed to be optional
* relationships updates are *extremely* unoptimized
* Clean up and test filter inspecting code.
* Handle related values on delete
* Use ashton to validate interface opts, not just document them: Easy and important
* Make an automatic test suite that confirms that data layers behave the way
  they claim to behave, maybe.
* Perhaps, reverse relationships should eliminate the need to set destination field.
* When checking for filter inclusion, we should allow for `and` filters to each
  contain *part* of the filter, requiring that the whole thing is covered by all
  of the `and`s at least
* add `init` to checks, and error check their construction when building the DSL
* Support filtering side loads. Especially useful in authorization code?
* Booleans need to not support `nil` values. That has to be a different type.
  boolean filter/authorization logic is greatly enhanced if that is the case.
* Consider that authorization steps may eventually need to be able to branch.
  This may or may not be difficult :) 
* Allow a feature called `verify_after_write` that fetches the final attribute
  variable from data somehow. Authorization fetchers will need to take state as
  an argument or something like that, and maybe need to specify dependencies?.
* Validate that checks have the correct action type when compiling an action
* Make sure updating foreign key attributes behaves the same as setting a
  relationship, or just disallow having editable attributes for relationship fkeys
* Validate `dependencies` and `must_fetch` (all `must_fetch` with dependencies
  must have those dependencies as `must_fetch` also)
* Support branching/more complicated control flow in authorization steps
* The Authorization flow for creates/updates may be insufficient. Instead of
  adding requests if relationships/attributes are changing, we may instead want
  to embed that knowledge inside the sat solver itself. Basically a
  `relationship_foo_is_changing?` fact, *and*ed with the resulting conditions.
  I'm not even sure if thats possible though.
* We need to validate incoming attributes/relationships better.
* Side load authorization testing
* Validate that params on the way in are either all strings or all atoms
* Make it `rules: :none` (or something better) than `rules: false`
* Support `read_rules`, `create_rules`, `update_rules` for attributes/relationships
* Make an `Ash.Changeset` that is a superset of an ecto changeset
* Engine parallelization!
* Big optimization: If the filter *did* need to fetch anything, find a way to
  get that fetched value into the final query.
