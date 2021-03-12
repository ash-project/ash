defmodule Ash.Resource.Info do
  @moduledoc "Introspection for resources"

  alias Ash.Dsl.Extension

  @spec set_metadata(Ash.Resource.record(), map) :: Ash.Resource.record()
  def set_metadata(record, map) do
    %{record | __metadata__: Ash.Helpers.deep_merge_maps(record.__metadata__, map)}
  end

  @spec put_metadata(Ash.Resource.record(), atom, term) :: Ash.Resource.record()
  def put_metadata(record, key, term) do
    set_metadata(record, %{key => term})
  end

  @spec get_metadata(Ash.Resource.record(), atom | list(atom)) :: term
  def get_metadata(record, key_or_path) do
    get_in(record.__metadata__ || %{}, List.wrap(key_or_path))
  end

  @spec selected?(Ash.Resource.record(), atom) :: boolean
  def selected?(%resource{} = record, field) do
    case get_metadata(record, :selected) do
      nil ->
        attribute = Ash.Resource.Info.attribute(resource, field)

        attribute && (!attribute.private? || attribute.primary_key?)

      select ->
        if field in select do
          true
        else
          attribute = Ash.Resource.Info.attribute(resource, field)

          attribute && attribute.primary_key?
        end
    end
  end

  @spec interfaces(Ash.Resource.t()) :: [Ash.Resource.Interface.t()]
  def interfaces(resource) do
    Extension.get_entities(resource, [:code_interface])
  end

  @spec extensions(Ash.Resource.t()) :: [module]
  def extensions(resource) do
    Extension.get_persisted(resource, :extensions, [])
  end

  @spec embedded?(Ash.Resource.t()) :: boolean
  def embedded?(resource) do
    Extension.get_persisted(resource, :embedded?, false)
  end

  @spec description(Ash.Resource.t()) :: String.t() | nil
  def description(resource) do
    Extension.get_opt(resource, [:resource], :description, "no description")
  end

  @spec base_filter(Ash.Resource.t()) :: term
  def base_filter(resource) do
    Extension.get_opt(resource, [:resource], :base_filter, nil)
  end

  @spec default_context(Ash.Resource.t()) :: term
  def default_context(resource) do
    Extension.get_opt(resource, [:resource], :default_context, nil)
  end

  @doc "A list of identities for the resource"
  @spec identities(Ash.Resource.t()) :: [Ash.Resource.Identity.t()]
  def identities(resource) do
    Extension.get_entities(resource, [:identities])
  end

  @doc "A list of authorizers to be used when accessing"
  @spec authorizers(Ash.Resource.t()) :: [module]
  def authorizers(resource) do
    Extension.get_persisted(resource, :authorizers, [])
  end

  @doc "A list of notifiers to be used when accessing"
  @spec notifiers(Ash.Resource.t()) :: [module]
  def notifiers(resource) do
    Extension.get_persisted(resource, :notifiers, [])
  end

  @spec validations(Ash.Resource.t(), :create | :update | :destroy) :: [
          Ash.Resource.Validation.t()
        ]
  def validations(resource, type) do
    resource
    |> validations()
    |> Enum.filter(&(type in &1.on))
  end

  @doc "A list of all validations for the resource"
  @spec validations(Ash.Resource.t()) :: [Ash.Resource.Validation.t()]
  def validations(resource) do
    Extension.get_entities(resource, [:validations])
  end

  @doc "Whether or not a given module is a resource module"
  @spec resource?(module) :: boolean
  def resource?(module) when is_atom(module) do
    # Prevent compile time dependency
    Ash.Dsl.is?(module, Module.concat(["Ash", Resource]))
  end

  def resource?(_), do: false

  @doc "A list of field names corresponding to the primary key"
  @spec primary_key(Ash.Resource.t()) :: list(atom)
  def primary_key(resource) do
    Ash.Dsl.Extension.get_persisted(resource, :primary_key, [])
  end

  @doc "Returns all relationships of a resource"
  @spec relationships(Ash.Resource.t()) :: list(Ash.Resource.Relationships.relationship())
  def relationships(resource) do
    Extension.get_entities(resource, [:relationships])
  end

  @doc "Get a relationship by name or path"
  @spec relationship(Ash.Resource.t(), atom | String.t() | [atom | String.t()]) ::
          Ash.Resource.Relationships.relationship() | nil
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

  def relationship(resource, relationship_name) when is_binary(relationship_name) do
    resource
    |> relationships()
    |> Enum.find(&(to_string(&1.name) == relationship_name))
  end

  def relationship(resource, relationship_name) do
    resource
    |> relationships()
    |> Enum.find(&(&1.name == relationship_name))
  end

  @doc "Returns all public relationships of a resource"
  @spec public_relationships(Ash.Resource.t()) :: list(Ash.Resource.Relationships.relationship())
  def public_relationships(resource) do
    resource
    |> relationships()
    |> Enum.reject(& &1.private?)
  end

  @doc "Get a public relationship by name or path"
  def public_relationship(resource, [name | rest]) do
    case public_relationship(resource, name) do
      nil ->
        nil

      relationship ->
        public_relationship(relationship.destination, rest)
    end
  end

  def public_relationship(resource, relationship_name) when is_binary(relationship_name) do
    resource
    |> relationships()
    |> Enum.find(&(to_string(&1.name) == relationship_name && !&1.private?))
  end

  def public_relationship(resource, relationship_name) do
    resource
    |> relationships()
    |> Enum.find(&(&1.name == relationship_name && !&1.private?))
  end

  @doc "Get the multitenancy strategy for a resource"
  @spec multitenancy_strategy(Ash.Resource.t()) :: :context | :attribute | nil
  def multitenancy_strategy(resource) do
    Ash.Dsl.Extension.get_opt(resource, [:multitenancy], :strategy, nil)
  end

  @spec multitenancy_attribute(Ash.Resource.t()) :: atom | nil
  def multitenancy_attribute(resource) do
    Ash.Dsl.Extension.get_opt(resource, [:multitenancy], :attribute, nil)
  end

  @spec multitenancy_parse_attribute(Ash.Resource.t()) :: {atom, atom, list(any)}
  def multitenancy_parse_attribute(resource) do
    Ash.Dsl.Extension.get_opt(
      resource,
      [:multitenancy],
      :parse_attribute,
      {__MODULE__, :identity, []}
    )
  end

  @doc false
  def identity(x), do: x

  @spec multitenancy_global?(Ash.Resource.t()) :: atom | nil
  def multitenancy_global?(resource) do
    Ash.Dsl.Extension.get_opt(resource, [:multitenancy], :global?, nil)
  end

  @spec multitenancy_source(Ash.Resource.t()) :: atom | nil
  def multitenancy_source(resource) do
    Ash.Dsl.Extension.get_opt(resource, [:multitenancy], :source, nil)
  end

  @spec multitenancy_template(Ash.Resource.t()) :: atom | nil
  def multitenancy_template(resource) do
    Ash.Dsl.Extension.get_opt(resource, [:multitenancy], :template, nil)
  end

  @doc "Returns all calculations of a resource"
  @spec calculations(Ash.Resource.t()) :: list(Ash.Resource.Calculation.t())
  def calculations(resource) do
    Extension.get_entities(resource, [:calculations])
  end

  @doc "Get a calculation by name"
  @spec calculation(Ash.Resource.t(), atom | String.t()) :: Ash.Resource.Calculation.t() | nil
  def calculation(resource, name) when is_binary(name) do
    resource
    |> calculations()
    |> Enum.find(&(to_string(&1.name) == name))
  end

  def calculation(resource, name) do
    resource
    |> calculations()
    |> Enum.find(&(&1.name == name))
  end

  @doc "Returns all public calculations of a resource"
  @spec public_calculations(Ash.Resource.t()) :: list(Ash.Resource.Calculation.t())
  def public_calculations(resource) do
    resource
    |> Extension.get_entities([:calculations])
    |> Enum.reject(& &1.private?)
  end

  @doc "Get a public calculation by name"
  @spec public_calculation(Ash.Resource.t(), atom | String.t()) ::
          Ash.Resource.Calculation.t() | nil
  def public_calculation(resource, name) when is_binary(name) do
    resource
    |> calculations()
    |> Enum.find(&(to_string(&1.name) == name && !&1.private?))
  end

  def public_calculation(resource, name) do
    resource
    |> calculations()
    |> Enum.find(&(&1.name == name && !&1.private?))
  end

  @doc "Returns all aggregates of a resource"
  @spec aggregates(Ash.Resource.t()) :: list(Ash.Resource.Aggregate.t())
  def aggregates(resource) do
    Extension.get_entities(resource, [:aggregates])
  end

  @doc "Get an aggregate by name"
  @spec aggregate(Ash.Resource.t(), atom | String.t()) :: Ash.Resource.Aggregate.t() | nil
  def aggregate(resource, name) when is_binary(name) do
    resource
    |> aggregates()
    |> Enum.find(&(to_string(&1.name) == name))
  end

  def aggregate(resource, name) do
    resource
    |> aggregates()
    |> Enum.find(&(&1.name == name))
  end

  @doc "Returns all public aggregates of a resource"
  @spec public_aggregates(Ash.Resource.t()) :: list(Ash.Resource.Aggregate.t())
  def public_aggregates(resource) do
    resource
    |> Extension.get_entities([:aggregates])
    |> Enum.reject(& &1.private?)
  end

  @doc "Get an aggregate by name"
  @spec public_aggregate(Ash.Resource.t(), atom | String.t()) ::
          Ash.Resource.Aggregate.t() | nil
  def public_aggregate(resource, name) when is_binary(name) do
    resource
    |> aggregates()
    |> Enum.find(&(to_string(&1.name) == name && !&1.private?))
  end

  def public_aggregate(resource, name) do
    resource
    |> aggregates()
    |> Enum.find(&(&1.name == name && !&1.private?))
  end

  @doc "Returns the primary action of the given type"
  @spec primary_action!(Ash.Resource.t(), Ash.Resource.Actions.action_type()) ::
          Ash.Resource.Actions.action() | no_return
  def primary_action!(resource, type) do
    case primary_action(resource, type) do
      nil -> raise "Required primary #{type} action for #{inspect(resource)}"
      action -> action
    end
  end

  @doc "Returns the primary action of a given type"
  @spec primary_action(Ash.Resource.t(), Ash.Resource.Actions.action_type()) ::
          Ash.Resource.Actions.action() | nil
  def primary_action(resource, type) do
    resource
    |> actions()
    |> Enum.filter(&(&1.type == type))
    |> case do
      [action] -> action
      actions -> Enum.find(actions, & &1.primary?)
    end
  end

  @doc "Returns the configured default actions"
  @spec default_actions(Ash.Resource.t()) :: [:create | :read | :update | :destroy]
  def default_actions(resource) do
    Extension.get_opt(
      resource,
      [:actions],
      :defaults,
      [:create, :read, :update, :destroy]
    )
  end

  @doc "Returns all actions of a resource"
  @spec actions(Ash.Resource.t()) :: [Ash.Resource.Actions.action()]
  def actions(resource) do
    Extension.get_entities(resource, [:actions])
  end

  @doc "Returns the action with the matching name and type on the resource"
  @spec action(Ash.Resource.t(), atom(), Ash.Resource.Actions.action_type() | nil) ::
          Ash.Resource.Actions.action() | nil
  def action(resource, name, _type \\ nil) do
    # We used to need type, but we don't anymore since action names are unique
    resource
    |> actions()
    |> Enum.find(&(&1.name == name))
  end

  @doc "Returns all attributes of a resource"
  @spec attributes(Ash.Resource.t()) :: [Ash.Resource.Attribute.t()]
  def attributes(resource) do
    Extension.get_entities(resource, [:attributes])
  end

  @doc "Get an attribute name from the resource"
  @spec attribute(Ash.Resource.t(), String.t() | atom) :: Ash.Resource.Attribute.t() | nil
  def attribute(resource, name) when is_binary(name) do
    resource
    |> attributes()
    |> Enum.find(&(to_string(&1.name) == name))
  end

  def attribute(resource, name) do
    resource
    |> attributes()
    |> Enum.find(&(&1.name == name))
  end

  @doc "Returns all public attributes of a resource"
  @spec public_attributes(Ash.Resource.t()) :: [Ash.Resource.Attribute.t()]
  def public_attributes(resource) do
    resource
    |> attributes()
    |> Enum.reject(& &1.private?)
  end

  @doc "Get a public attribute name from the resource"
  @spec public_attribute(Ash.Resource.t(), String.t() | atom) :: Ash.Resource.Attribute.t() | nil
  def public_attribute(resource, name) when is_binary(name) do
    resource
    |> attributes()
    |> Enum.find(&(to_string(&1.name) == name && !&1.private?))
  end

  def public_attribute(resource, name) do
    resource
    |> attributes()
    |> Enum.find(&(&1.name == name && !&1.private?))
  end

  @spec related(Ash.Resource.t(), atom() | String.t() | [atom() | String.t()]) ::
          Ash.Resource.t() | nil
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
end
