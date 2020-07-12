defmodule Ash do
  @moduledoc """
  Ash Framework

  ![Logo](https://github.com/ash-project/ash/blob/master/logos/cropped-for-header.png?raw=true)

  ## Quick Links

  - [Resource Documentation](Ash.Resource.html)
  - [DSL Documentation](Ash.Dsl.html)
  - [Code API documentation](Ash.Api.Interface.html)
  - [Getting Started Guide](getting_started.html)

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

  @type record :: struct
  @type relationship_cardinality :: :many | :one
  @type cardinality_one_relationship() :: HasOne.t() | BelongsTo.t()
  @type cardinality_many_relationship() :: HasMany.t() | ManyToMany.t()
  @type relationship :: cardinality_one_relationship() | cardinality_many_relationship()
  @type resource :: module
  @type data_layer :: module
  @type data_layer_query :: struct
  @type api :: module
  @type error :: struct
  @type filter :: map()
  @type params :: Keyword.t()
  @type sort :: Keyword.t()
  @type side_loads :: Keyword.t()
  @type attribute :: Ash.Resource.Attribute.t()
  @type action :: Create.t() | Read.t() | Update.t() | Destroy.t()
  @type query :: Ash.Query.t()
  @type actor :: Ash.record()
  @type changeset :: Ash.Changeset.t()

  require Ash.Dsl.Extension
  alias Ash.Dsl.Extension

  @doc "A list of authorizers to be used when accessing the resource"
  @spec authorizers(resource()) :: [module]
  def authorizers(resource) do
    {resource, :authorizers}
    |> :persistent_term.get([])
    |> List.wrap()
  end

  @doc "A list of field names corresponding to the primary key of a resource"
  @spec primary_key(resource()) :: list(atom)
  def primary_key(resource) do
    :persistent_term.get({resource, :primary_key}, [])
  end

  def relationships(resource) do
    Extension.get_entities(resource, [:relationships])
  end

  @spec relationship(any, any) :: any
  @doc "Gets a relationship by name from the resource"
  def relationship(resource, [name]) do
    relationship(resource, name)
  end

  def relationship(resource, [name | rest]) do
    case relationship(resource, name) do
      nil ->
        nil

      relationship ->
        relationship(relationship.destination, rest)
    end
  end

  def relationship(resource, relationship_name) when is_bitstring(relationship_name) do
    resource
    |> relationships()
    |> Enum.find(&(to_string(&1.name) == relationship_name))
  end

  def relationship(resource, relationship_name) do
    resource
    |> relationships()
    |> Enum.find(&(&1.name == relationship_name))
  end

  def implements_behaviour?(module, behaviour) do
    :attributes
    |> module.module_info()
    |> Enum.flat_map(fn
      {:behaviour, value} -> List.wrap(value)
      _ -> []
    end)
    |> Enum.any?(&(&1 == behaviour))
  end

  @doc false
  def primary_action!(resource, type) do
    case primary_action(resource, type) do
      nil -> raise "Required primary #{type} action for #{inspect(resource)}"
      action -> action
    end
  end

  @doc "Returns the primary action of a given type for a resource"
  @spec primary_action(resource(), atom()) :: action() | nil
  def primary_action(resource, type) do
    resource
    |> actions()
    |> Enum.filter(&(&1.type == type))
    |> case do
      [action] -> action
      actions -> Enum.find(actions, & &1.primary?)
    end
  end

  def actions(resource) do
    Extension.get_entities(resource, [:actions])
  end

  @doc "Returns the action with the matching name and type on the resource"
  @spec action(resource(), atom(), atom()) :: action() | nil
  def action(resource, name, type) do
    resource
    |> actions()
    |> Enum.find(&(&1.name == name && &1.type == type))
  end

  def attributes(resource) do
    Extension.get_entities(resource, [:attributes])
  end

  def extensions(resource) do
    :persistent_term.get({resource, :extensions}, [])
  end

  @doc "Get an attribute name from the resource"
  @spec attribute(resource(), String.t() | atom) :: attribute() | nil
  def attribute(resource, name) when is_bitstring(name) do
    resource
    |> attributes()
    |> Enum.find(&(to_string(&1.name) == name))
  end

  def attribute(resource, name) do
    resource
    |> attributes()
    |> Enum.find(&(&1.name == name))
  end

  def related(resource, relationship) when not is_list(relationship) do
    related(resource, [relationship])
  end

  def related(resource, []), do: resource

  def related(resource, [path | rest]) do
    case relationship(resource, path) do
      %{destination: destination} -> related(destination, rest)
      nil -> nil
    end
  end

  @doc "The data layer of the resource, or nil if it does not have one"
  @spec data_layer(resource()) :: data_layer()
  def data_layer(resource) do
    :persistent_term.get({resource, :data_layer})
  end

  @doc false
  @spec data_layer_can?(resource(), Ash.DataLayer.feature()) :: boolean
  def data_layer_can?(resource, feature) do
    data_layer = data_layer(resource)

    data_layer && Ash.DataLayer.can?(feature, resource)
  end

  @doc false
  @spec data_layer_filters(resource) :: map
  def data_layer_filters(resource) do
    Ash.DataLayer.custom_filters(resource)
  end

  @spec in_transaction?(resource) :: boolean
  def in_transaction?(resource) do
    data_layer(resource).in_transaction?(resource)
  end

  @spec transaction(resource, (() -> term)) :: term
  def transaction(resource, func) do
    if data_layer_can?(resource, :transact) do
      data_layer(resource).transaction(resource, func)
    else
      func.()
    end
  end

  @spec rollback(resource, term) :: no_return
  def rollback(resource, term) do
    data_layer(resource).rollback(resource, term)
  end
end
