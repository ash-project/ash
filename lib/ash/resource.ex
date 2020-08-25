defmodule Ash.Resource do
  @moduledoc """
  A resource is a static definition of an entity in your system.

  Resource DSL documentation: `Ash.Resource.Dsl`
  """

  alias Ash.Dsl.Extension

  defmacro __using__(opts) do
    data_layer = Macro.expand(opts[:data_layer], __CALLER__)

    authorizers =
      opts[:authorizers]
      |> List.wrap()
      |> Enum.map(&Macro.expand(&1, __CALLER__))

    extensions =
      if data_layer && Ash.implements_behaviour?(data_layer, Ash.Dsl.Extension) do
        [data_layer, Ash.Resource.Dsl]
      else
        [Ash.Resource.Dsl]
      end

    authorizer_extensions =
      Enum.filter(authorizers, &Ash.implements_behaviour?(&1, Ash.Dsl.Extension))

    extensions = Enum.concat([extensions, opts[:extensions] || [], authorizer_extensions])

    body =
      quote bind_quoted: [opts: opts] do
        @before_compile Ash.Resource

        @authorizers opts[:authorizers] || []
        @data_layer opts[:data_layer]
        @extensions (opts[:extensions] || []) ++
                      List.wrap(opts[:data_layer]) ++ (opts[:authorizers] || [])
      end

    preparations = Extension.prepare(extensions)

    [body | preparations]
  end

  # credo:disable-for-next-line Credo.Check.Refactor.CyclomaticComplexity
  defmacro __before_compile__(_env) do
    quote do
      @doc false
      alias Ash.Dsl.Extension

      Module.register_attribute(__MODULE__, :is_ash_resource, persist: true, accumulate: false)
      @is_ash_resource true

      @on_load :on_load

      :persistent_term.put({__MODULE__, :data_layer}, @data_layer)
      :persistent_term.put({__MODULE__, :authorizers}, @authorizers)
      :persistent_term.put({__MODULE__, :extensions}, @extensions)

      @ash_dsl_config Extension.set_state()

      def on_load do
        :persistent_term.put({__MODULE__, :data_layer}, @data_layer)
        :persistent_term.put({__MODULE__, :authorizers}, @authorizers)
        :persistent_term.put({__MODULE__, :extensions}, @extensions)

        Extension.load()
      end

      require Ash.Schema

      Ash.Schema.define_schema()
    end
  end

  def extensions(resource) do
    :persistent_term.get({resource, :extensions})
  end

  @spec description(Ash.resource()) :: String.t() | nil
  def description(resource) do
    Extension.get_opt(resource, [:resource], :description, "no description")
  end

  @doc "A list of authorizers to be used when accessing"
  @spec authorizers(Ash.resource()) :: [module]
  def authorizers(resource) do
    {resource, :authorizers}
    |> :persistent_term.get([])
    |> List.wrap()
  end

  @spec validations(Ash.resource(), :create | :update | :destroy) :: [Ash.validation()]
  def validations(resource, type) do
    resource
    |> validations()
    |> Enum.filter(&(type in &1.on))
  end

  @doc "A list of all validations for the resource"
  @spec validations(Ash.resource()) :: [Ash.validation()]
  def validations(resource) do
    Extension.get_entities(resource, [:validations])
  end

  @doc "Whether or not a given module is a resource module"
  @spec resource?(module) :: boolean
  def resource?(module) when is_atom(module) do
    module.module_info(:attributes)[:is_ash_resource] == [true]
  end

  def resource?(_), do: false

  @doc "A list of field names corresponding to the primary key"
  @spec primary_key(Ash.resource()) :: list(atom)
  def primary_key(resource) do
    :persistent_term.get({resource, :primary_key}, [])
  end

  @spec relationships(Ash.resource()) :: list(Ash.relationship())
  def relationships(resource) do
    Extension.get_entities(resource, [:relationships])
  end

  @doc "Get a relationship by name or path"
  @spec relationship(Ash.resource(), atom | [atom]) :: Ash.relationship() | nil
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

  @spec calculations(Ash.resource()) :: list(Ash.calculation())
  def calculations(resource) do
    Extension.get_entities(resource, [:calculations])
  end

  def calculation(resource, name) when is_bitstring(name) do
    resource
    |> calculations()
    |> Enum.find(&(to_string(&1.name) == name))
  end

  def calculation(resource, name) do
    resource
    |> calculations()
    |> Enum.find(&(&1.name == name))
  end

  @spec aggregates(Ash.resource()) :: list(Ash.aggregate())
  def aggregates(resource) do
    Extension.get_entities(resource, [:aggregates])
  end

  def aggregate(resource, name) when is_bitstring(name) do
    resource
    |> aggregates()
    |> Enum.find(&(to_string(&1.name) == name))
  end

  def aggregate(resource, name) do
    resource
    |> aggregates()
    |> Enum.find(&(&1.name == name))
  end

  @doc "Returns the primary action of the given type"
  @spec primary_action!(Ash.resource(), Ash.action_type()) :: Ash.action() | no_return
  def primary_action!(resource, type) do
    case primary_action(resource, type) do
      nil -> raise "Required primary #{type} action for #{inspect(resource)}"
      action -> action
    end
  end

  @doc "Returns the primary action of a given type"
  @spec primary_action(Ash.resource(), Ash.action_type()) :: Ash.action() | nil
  def primary_action(resource, type) do
    resource
    |> actions()
    |> Enum.filter(&(&1.type == type))
    |> case do
      [action] -> action
      actions -> Enum.find(actions, & &1.primary?)
    end
  end

  @doc "Returns all actions of a resource"
  @spec actions(Ash.resource()) :: [Ash.action()]
  def actions(resource) do
    Extension.get_entities(resource, [:actions])
  end

  @doc "Returns the action with the matching name and type on the resource"
  @spec action(Ash.resource(), atom(), Ash.action_type()) :: Ash.action() | nil
  def action(resource, name, type) do
    resource
    |> actions()
    |> Enum.find(&(&1.name == name && &1.type == type))
  end

  @doc "Returns all attributes of a resource"
  @spec attributes(Ash.resource()) :: [Ash.attribute()]
  def attributes(resource) do
    Extension.get_entities(resource, [:attributes])
  end

  @doc "Get an attribute name from the resource"
  @spec attribute(Ash.resource(), String.t() | atom) :: Ash.attribute() | nil
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

  @spec related(Ash.resource(), atom() | String.t() | [atom() | String.t()]) ::
          Ash.resource() | nil
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
  @spec data_layer(Ash.resource()) :: Ash.data_layer()
  def data_layer(resource) do
    :persistent_term.get({resource, :data_layer})
  end

  @doc "Whether ornot the data layer supports a specific feature"
  @spec data_layer_can?(Ash.resource(), Ash.DataLayer.feature()) :: boolean
  def data_layer_can?(resource, feature) do
    data_layer = data_layer(resource)

    data_layer && Ash.DataLayer.can?(feature, resource)
  end

  @doc "Custom filters suppoted by the data layer of the resource"
  @spec data_layer_filters(Ash.resource()) :: map
  def data_layer_filters(resource) do
    Ash.DataLayer.custom_filters(resource)
  end

  @doc "Whether or not the data layer for the resource is currently in a transaction"
  @spec in_transaction?(Ash.resource()) :: boolean
  def in_transaction?(resource) do
    data_layer(resource).in_transaction?(resource)
  end

  @doc "Wraps the execution of the function in a transaction with the resource's datalayer"
  @spec transaction(Ash.resource(), (() -> term)) :: term
  def transaction(resource, func) do
    if data_layer_can?(resource, :transact) do
      data_layer(resource).transaction(resource, func)
    else
      func.()
    end
  end

  @doc "Rolls back the current transaction"
  @spec rollback(Ash.resource(), term) :: no_return
  def rollback(resource, term) do
    data_layer(resource).rollback(resource, term)
  end
end
