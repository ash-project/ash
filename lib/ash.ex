defmodule Ash do
  @moduledoc """
  Ash Framework

  ![Logo](https://github.com/ash-project/ash/blob/master/logos/cropped-for-header.png?raw=true)

  ## ALPHA NOTICE

  Ash is in alpha. The package version is 1.0.0+, and most of the time that means stable, but in this case it _does not_. The 2.0 release will be the stable release.

  ## Quick Links

  - [Resource DSL Documentation](Ash.Resource.Dsl.html)
  - [Api DSL Documentation](Ash.Api.Dsl.html)
  - [Api interface documentation](Ash.Api.html)
  - [Query Documentation](Ash.Query.html)
  - [Changeset Documentation](Ash.Changeset.html)
  - [Guides](getting_started.html)

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

  See the [getting started guide](getting_started.html) for more information.

  For those looking to add ash extensions:

  * see `Ash.Dsl.Extension` for adding configuration.
  * If you are looking to write a new data source, also see the `Ash.DataLayer` documentation.
  * If you are looking to write a new authorizer, see `Ash.Authorizer`
  * If you are looking to write a "front end", something powered by Ash resources, a guide on
  building those kinds of tools is in the works.
  """
  alias Ash.Resource.Actions.{Create, Destroy, Read, Update}
  alias Ash.Resource.Relationships.{BelongsTo, HasMany, HasOne, ManyToMany}

  @type action :: Create.t() | Read.t() | Update.t() | Destroy.t()
  @type action_type :: :read | :create | :update | :destroy
  @type actor :: Ash.record()
  @type aggregate :: Ash.Query.Aggregate.t() | Ash.Resource.Aggregate.t()
  @type aggregate_kind :: Ash.Query.Aggregate.kind()
  @type api :: module
  @type attribute :: Ash.Resource.Attribute.t()
  @type calculation :: Ash.Resource.Calculation.t()
  @type cardinality_many_relationship() :: HasMany.t() | ManyToMany.t()
  @type cardinality_one_relationship() :: HasOne.t() | BelongsTo.t()
  @type changeset :: Ash.Changeset.t()
  @type data_layer :: module
  @type data_layer_query :: struct
  @type error :: struct
  @type filter :: Ash.Filter.t()
  @type params :: Keyword.t()
  @type primary_key :: record() | map | term
  @type query :: Ash.Query.t()
  @type record :: struct
  @type relationship :: cardinality_one_relationship() | cardinality_many_relationship()
  @type relationship_cardinality :: :many | :one
  @type resource :: module
  @type side_loads :: term
  @type sort :: Keyword.t()
  @type validation :: Ash.Resource.Validation.t()

  require Ash.Dsl.Extension

  def implements_behaviour?(module, behaviour) do
    :attributes
    |> module.module_info()
    |> Enum.flat_map(fn
      {:behaviour, value} -> List.wrap(value)
      _ -> []
    end)
    |> Enum.any?(&(&1 == behaviour))
  end

  def uuid do
    Ecto.UUID.generate()
  end

  @doc "Returns all extensions of a resource or api"
  @spec extensions(resource() | api()) :: [module]
  def extensions(resource) do
    :persistent_term.get({resource, :extensions}, [])
  end
end
