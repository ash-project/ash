defmodule Ash.Resource.Info do
  @moduledoc "Introspection for resources"

  alias Spark.Dsl.Extension

  @doc """
  Returns the statically configured domain for the resource.
  """
  def domain(resource) do
    Spark.Dsl.Extension.get_persisted(resource, :domain)
  end

  @doc """
  Retrieves a relationship path from the resource related by path, to the provided resource.
  """
  def reverse_relationship(resource, path, acc \\ [])

  def reverse_relationship(_, [], acc),
    do: acc

  def reverse_relationship(resource, [name | rest], acc) do
    resource
    |> relationships()
    |> Enum.find(fn relationship ->
      relationship.name == name
    end)
    |> case do
      nil ->
        nil

      relationship ->
        relationship.destination
        |> relationships()
        |> Enum.find(fn candidate ->
          reverse_relationship?(relationship, candidate)
        end)
        |> case do
          nil ->
            nil

          destination_relationship ->
            reverse_relationship(relationship.destination, rest, [
              destination_relationship.name | acc
            ])
        end
    end
  end

  defp reverse_relationship?(rel, destination_rel) do
    rel.source == destination_rel.destination &&
      rel.destination == destination_rel.source &&
      rel.source_attribute == destination_rel.destination_attribute &&
      rel.destination_attribute == destination_rel.source_attribute &&
      Map.fetch(rel, :source_attribute_on_join_resource) ==
        Map.fetch(destination_rel, :destination_attribute_on_join_resource) &&
      Map.fetch(rel, :destination_attribute_on_join_resource) ==
        Map.fetch(destination_rel, :source_attribute_on_join_resource) &&
      is_nil(destination_rel.context) &&
      is_nil(rel.context)
  end

  @doc """
  The list of code interface definitions.
  """
  @spec interfaces(Spark.Dsl.t() | Ash.Resource.t()) :: [Ash.Resource.Interface.t()]
  def interfaces(resource) do
    resource
    |> Extension.get_entities([:code_interface])
    |> Enum.filter(fn
      %Ash.Resource.Interface{} ->
        true

      _ ->
        false
    end)
  end

  @doc "Get an interface by name from the resource"
  @spec interface(Spark.Dsl.t() | Ash.Resource.t(), atom) :: Ash.Resource.Interface.t() | nil
  def interface(resource, name) do
    resource
    |> interfaces()
    |> Enum.find(&(&1.name == name))
  end

  @doc """
  The list of code interface calculation definitions.
  """
  @spec calculation_interfaces(Spark.Dsl.t() | Ash.Resource.t()) :: [
          Ash.Resource.CalculationInterface.t()
        ]
  def calculation_interfaces(resource) do
    resource
    |> Extension.get_entities([:code_interface])
    |> Enum.filter(fn
      %Ash.Resource.CalculationInterface{} ->
        true

      _ ->
        false
    end)
  end

  @doc "Get an calculation interface by name from the resource"
  @spec calculation_interface(Spark.Dsl.t() | Ash.Resource.t(), atom) ::
          Ash.Resource.CalculationInterface.t() | nil
  def calculation_interface(resource, name) do
    resource
    |> calculation_interfaces()
    |> Enum.find(&(&1.name == name))
  end

  @doc """
  The domain to define the interface for, when defining it in the resource
  """
  @spec code_interface_domain(Spark.Dsl.t() | Ash.Resource.t()) :: atom | nil
  def code_interface_domain(resource) do
    Extension.get_opt(resource, [:code_interface], :domain, nil)
  end

  @doc """
  Whether or not to define the interface on the resource
  """
  @spec define_interface?(Spark.Dsl.t() | Ash.Resource.t()) :: boolean
  def define_interface?(resource) do
    Extension.get_opt(resource, [:code_interface], :define?, true)
  end

  @doc """
  Whether or not the resource is an embedded resource
  """
  @spec embedded?(Spark.Dsl.t() | Ash.Resource.t()) :: boolean
  def embedded?(resource) do
    Extension.get_persisted(resource, :embedded?, false)
  end

  @doc """
  The description of the resource
  """
  @spec description(Spark.Dsl.t() | Ash.Resource.t()) :: String.t() | nil
  def description(resource) do
    Extension.get_opt(resource, [:resource], :description, nil)
  end

  @doc """
  A list of simple notifiers (require no DSL, used to avoid compile time dependencies)
  """
  @spec simple_notifiers(Spark.Dsl.t() | Ash.Resource.t()) :: list(module)
  def simple_notifiers(resource) do
    Extension.get_persisted(resource, :simple_notifiers, [])
  end

  @doc """
  The trace_name of the resource
  """
  @spec trace_name(Spark.Dsl.t() | Ash.Resource.t()) :: String.t() | nil
  def trace_name(resource) do
    Extension.get_opt(resource, [:resource], :trace_name, nil) || to_string(short_name(resource))
  end

  @doc """
  The short_name of the resource
  """
  # sobelow_skip ["DOS.StringToAtom"]
  @spec short_name(Spark.Dsl.t() | Ash.Resource.t()) :: atom | nil
  def short_name(resource) when is_map(resource) do
    Extension.get_opt(resource, [:resource], :short_name, nil) ||
      resource
      |> Extension.get_persisted(:module)
      |> Module.split()
      |> List.last()
      |> Macro.underscore()
      |> String.to_atom()
  end

  def short_name(resource) do
    Extension.get_opt(resource, [:resource], :short_name, nil) || resource.default_short_name()
  end

  @doc """
  The plural_name of the resource
  """
  def plural_name(resource) do
    Extension.get_opt(resource, [:resource], :plural_name, nil)
  end

  @doc """
  The base filter of the resource
  """
  @spec base_filter(Spark.Dsl.t() | Ash.Resource.t()) :: term
  def base_filter(resource) do
    Extension.get_opt(resource, [:resource], :base_filter, nil)
  end

  @doc """
  The default context of the resource
  """
  @spec default_context(Spark.Dsl.t() | Ash.Resource.t()) :: term
  def default_context(resource) do
    Extension.get_opt(resource, [:resource], :default_context, nil)
  end

  @doc "A list of identities for the resource"
  @spec identities(Spark.Dsl.t() | Ash.Resource.t()) :: [Ash.Resource.Identity.t()]
  def identities(resource) do
    Extension.get_entities(resource, [:identities])
  end

  @doc "Get an identity by name from the resource"
  @spec identity(Spark.Dsl.t() | Ash.Resource.t(), atom) :: Ash.Resource.Identity.t() | nil
  def identity(resource, name) do
    resource
    |> identities()
    |> Enum.find(&(&1.name == name))
  end

  @doc "A list of authorizers to be used when accessing"
  @spec authorizers(Spark.Dsl.t() | Ash.Resource.t()) :: [module]
  def authorizers(resource) do
    Extension.get_persisted(resource, :authorizers, [])
  end

  @doc "A list of notifiers to be used when accessing"
  @spec notifiers(Spark.Dsl.t() | Ash.Resource.t()) :: [module]
  def notifiers(resource) do
    Extension.get_persisted(resource, :notifiers, []) ++ simple_notifiers(resource)
  end

  @doc "A list of all validations for the resource for a given action type"
  @spec validations(Spark.Dsl.t() | Ash.Resource.t(), :create | :update | :destroy) :: [
          Ash.Resource.Validation.t()
        ]
  def validations(resource, type) do
    Extension.get_persisted(resource, :validations_by_on)[type] ||
      resource
      |> validations()
      |> Enum.filter(&(type in &1.on))
  end

  @doc "A list of all validations for the resource"
  @spec validations(Spark.Dsl.t() | Ash.Resource.t()) :: [Ash.Resource.Validation.t()]
  def validations(resource) do
    Extension.get_entities(resource, [:validations])
  end

  @doc "A list of all changes for the resource for a given action type"
  @spec changes(Spark.Dsl.t() | Ash.Resource.t(), :create | :update | :destroy) ::
          list(
            Ash.Resource.Validation.t()
            | Ash.Resource.Change.t()
          )
  def changes(resource, type) do
    Extension.get_persisted(resource, :changes_by_on)[type] ||
      resource
      |> changes()
      |> Enum.filter(&(type in &1.on))
  end

  @doc "A list of all changes for the resource"
  @spec changes(Spark.Dsl.t() | Ash.Resource.t()) ::
          list(Ash.Resource.Validation.t() | Ash.Resource.Change.t())
  def changes(resource) do
    Extension.get_entities(resource, [:changes])
  end

  @spec preparations(Spark.Dsl.t() | Ash.Resource.t()) :: list(Ash.Resource.Preparation.t())
  def preparations(resource) do
    Extension.get_entities(resource, [:preparations])
  end

  @doc "Whether or not a given module is a resource module"
  @spec resource?(module) :: boolean
  def resource?(module) when is_atom(module) do
    Spark.Dsl.is?(module, Ash.Resource)
  end

  def resource?(_), do: false

  @doc "A list of field names corresponding to the primary key"
  @spec primary_key(Spark.Dsl.t() | Ash.Resource.t()) :: list(atom)
  def primary_key(resource) do
    Spark.Dsl.Extension.get_persisted(resource, :primary_key, [])
  end

  @doc "A list of unique keys and information for a resource"
  @spec unique_keys(Spark.Dsl.t() | Ash.Resource.t()) ::
          list(%{type: atom, keys: list(atom), nils_distinct?: boolean()})
  def unique_keys(resource) do
    Spark.Dsl.Extension.get_persisted(resource, :unique_keys, [])
  end

  @doc "Whether or not all primary key attributes can be compared with simple_equality"
  @spec primary_key_simple_equality?(Spark.Dsl.t() | Ash.Resource.t()) :: boolean()
  def primary_key_simple_equality?(resource) do
    Spark.Dsl.Extension.get_persisted(resource, :primary_key_simple_equality?, [])
  end

  @doc "Returns all relationships of a resource"
  @spec relationships(Spark.Dsl.t() | Ash.Resource.t()) ::
          list(Ash.Resource.Relationships.relationship())
  def relationships(resource) do
    Extension.get_entities(resource, [:relationships])
  end

  @doc "Get a relationship by name or path"
  @spec relationship(Spark.Dsl.t() | Ash.Resource.t(), atom | String.t() | [atom | String.t()]) ::
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
    Extension.get_persisted(resource, :relationships_by_name)[relationship_name] ||
      resource
      |> relationships()
      |> Enum.find(&(to_string(&1.name) == relationship_name))
  end

  def relationship(resource, relationship_name) do
    Extension.get_persisted(resource, :relationships_by_name)[relationship_name] ||
      resource
      |> relationships()
      |> Enum.find(&(&1.name == relationship_name))
  end

  @doc "Returns all public relationships of a resource"
  @spec public_relationships(Spark.Dsl.t() | Ash.Resource.t()) ::
          list(Ash.Resource.Relationships.relationship())
  def public_relationships(resource) do
    resource
    |> relationships()
    |> Enum.filter(& &1.public?)
  end

  @doc "The required belongs_to relationships"
  def required_belongs_to_relationships(resource) do
    Extension.get_persisted(resource, :required_belongs_to_relationships) ||
      Enum.filter(
        relationships(resource),
        &(&1.type == :belongs_to && !&1.allow_nil?)
      )
  end

  @doc "Get a public relationship by name or path"
  def public_relationship(resource, [name]) do
    public_relationship(resource, name)
  end

  def public_relationship(resource, [name | rest]) do
    case public_relationship(resource, name) do
      nil ->
        nil

      relationship ->
        public_relationship(relationship.destination, rest)
    end
  end

  def public_relationship(resource, relationship_name) do
    case relationship(resource, relationship_name) do
      %{public?: true} = relationship -> relationship
      _ -> nil
    end
  end

  @doc "The multitenancy strategy for a resource"
  @spec multitenancy_strategy(Spark.Dsl.t() | Ash.Resource.t()) :: :context | :attribute | nil
  def multitenancy_strategy(resource) do
    Spark.Dsl.Extension.get_opt(resource, [:multitenancy], :strategy, nil)
  end

  @doc "The multitenancy attribute for a resource"
  @spec multitenancy_attribute(Spark.Dsl.t() | Ash.Resource.t()) :: atom | nil
  def multitenancy_attribute(resource) do
    Spark.Dsl.Extension.get_opt(resource, [:multitenancy], :attribute, nil)
  end

  @doc "The function to parse the tenant from the attribute"
  @spec multitenancy_parse_attribute(Spark.Dsl.t() | Ash.Resource.t()) :: {atom, atom, list(any)}
  def multitenancy_parse_attribute(resource) do
    Spark.Dsl.Extension.get_opt(
      resource,
      [:multitenancy],
      :parse_attribute,
      {__MODULE__, :_identity, []}
    )
  end

  @doc false
  def _identity(x), do: x

  @doc "The MFA to parse the tenant from the attribute"
  @spec multitenancy_global?(Spark.Dsl.t() | Ash.Resource.t()) :: atom | nil
  def multitenancy_global?(resource) do
    Spark.Dsl.Extension.get_opt(resource, [:multitenancy], :global?, nil)
  end

  @doc "The template for creating the tenant name"
  @spec multitenancy_template(Spark.Dsl.t() | Ash.Resource.t()) :: atom | nil
  def multitenancy_template(resource) do
    Spark.Dsl.Extension.get_opt(resource, [:multitenancy], :template, nil)
  end

  @doc "Returns all calculations of a resource"
  @spec calculations(Spark.Dsl.t() | Ash.Resource.t()) :: list(Ash.Resource.Calculation.t())
  def calculations(resource) do
    Extension.get_entities(resource, [:calculations])
  end

  @doc "Get a calculation by name"
  @spec calculation(Spark.Dsl.t() | Ash.Resource.t(), atom | String.t()) ::
          Ash.Resource.Calculation.t() | nil
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
  @spec public_calculations(Spark.Dsl.t() | Ash.Resource.t()) ::
          list(Ash.Resource.Calculation.t())
  def public_calculations(resource) do
    resource
    |> Extension.get_entities([:calculations])
    |> Enum.filter(& &1.public?)
  end

  @doc "Get a public calculation by name"
  @spec public_calculation(Spark.Dsl.t() | Ash.Resource.t(), atom | String.t()) ::
          Ash.Resource.Calculation.t() | nil
  def public_calculation(resource, name) when is_binary(name) do
    resource
    |> calculations()
    |> Enum.find(&(to_string(&1.name) == name && &1.public?))
  end

  def public_calculation(resource, name) do
    resource
    |> calculations()
    |> Enum.find(&(&1.name == name && &1.public?))
  end

  @doc """
  Gets the type of an aggregate for a given resource.
  """
  @spec aggregate_type(Spark.Dsl.t() | Ash.Resource.t(), Ash.Resource.Aggregate.t() | atom) ::
          {:ok, Ash.Type.t()} | {:error, String.t()}
  def aggregate_type(resource, aggregate) when is_atom(aggregate) do
    aggregate_type(resource, aggregate(resource, aggregate))
  end

  def aggregate_type(resource, aggregate) do
    attribute =
      if aggregate.field do
        related = Ash.Resource.Info.related(resource, aggregate.relationship_path)
        Ash.Resource.Info.attribute(related, aggregate.field)
      end

    attribute_type =
      if attribute do
        attribute.type
      end

    attribute_constraints =
      if attribute do
        attribute.constraints
      end

    case Ash.Query.Aggregate.kind_to_type(aggregate.kind, attribute_type, attribute_constraints) do
      {:ok, type, _constraints} ->
        {:ok, type}

      other ->
        other
    end
  end

  @doc "Returns all aggregates of a resource"
  @spec aggregates(Spark.Dsl.t() | Ash.Resource.t()) :: list(Ash.Resource.Aggregate.t())
  def aggregates(resource) do
    Extension.get_entities(resource, [:aggregates])
  end

  @doc "Get an aggregate by name"
  @spec aggregate(Spark.Dsl.t() | Ash.Resource.t(), atom | String.t()) ::
          Ash.Resource.Aggregate.t() | nil
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
  @spec public_aggregates(Spark.Dsl.t() | Ash.Resource.t()) :: list(Ash.Resource.Aggregate.t())
  def public_aggregates(resource) do
    resource
    |> Extension.get_entities([:aggregates])
    |> Enum.filter(& &1.public?)
  end

  @doc "Get an aggregate by name"
  @spec public_aggregate(Spark.Dsl.t() | Ash.Resource.t(), atom | String.t()) ::
          Ash.Resource.Aggregate.t() | nil
  def public_aggregate(resource, name) when is_binary(name) do
    resource
    |> aggregates()
    |> Enum.find(&(to_string(&1.name) == name && &1.public?))
  end

  def public_aggregate(resource, name) do
    resource
    |> aggregates()
    |> Enum.find(&(&1.name == name && &1.public?))
  end

  @doc "Returns the primary action of the given type"
  @spec primary_action!(Spark.Dsl.t() | Ash.Resource.t(), Ash.Resource.Actions.action_type()) ::
          Ash.Resource.Actions.action() | no_return
  def primary_action!(resource, type) do
    case primary_action(resource, type) do
      nil ->
        raise "Required primary #{type} action for #{inspect(Extension.get_persisted(resource, :module))}."

      action ->
        action
    end
  end

  @doc "Returns the primary action of a given type"
  @spec primary_action(Spark.Dsl.t() | Ash.Resource.t(), Ash.Resource.Actions.action_type()) ::
          Ash.Resource.Actions.action() | nil
  def primary_action(resource, type) do
    resource
    |> actions()
    |> Enum.find(&(&1.type == type && &1.primary?))
  end

  @doc "Returns the configured default actions"
  @spec default_actions(Spark.Dsl.t() | Ash.Resource.t()) ::
          list(:create | :read | :update | :destroy)
  def default_actions(resource) do
    default =
      if embedded?(resource) do
        [:create, :read, :update, :destroy]
        |> Enum.reject(&Ash.Resource.Info.action(resource, &1))
      else
        []
      end

    Extension.get_opt(
      resource,
      [:actions],
      :defaults,
      default
    )
  end

  @doc "Returns all actions of a resource"
  @spec actions(Spark.Dsl.t() | Ash.Resource.t()) :: [Ash.Resource.Actions.action()]
  def actions(resource) do
    Extension.get_entities(resource, [:actions])
  end

  @doc "Returns the action with the matching name and type on the resource"
  @spec action(Spark.Dsl.t() | Ash.Resource.t(), atom(), Ash.Resource.Actions.action_type() | nil) ::
          Ash.Resource.Actions.action() | nil
  def action(resource, name, type \\ nil) do
    # We used to need type, but we don't anymore since action names are unique
    if type do
      resource
      |> actions()
      |> Enum.find(&(&1.name == name))
      |> case do
        nil ->
          nil

        %{type: ^type} = action ->
          action

        %{type: found_type} ->
          raise ArgumentError, """
          Found an action of type #{found_type} while looking for an action of type #{type}

          Perhaps you've passed a changeset with the incorrect action type?
          """
      end
    else
      resource
      |> actions()
      |> Enum.find(&(&1.name == name))
    end
  end

  @doc "Returns true or false if the input is accepted by the action, as an argument or an attribute"
  @spec action_input?(Ash.Resource.t(), action :: atom(), input :: atom() | String.t()) ::
          boolean()
  def action_input?(resource, action, input) do
    case Extension.get_persisted(resource, {:action_inputs, action}) do
      nil -> false
      map_set -> input in map_set
    end
  end

  @doc "Returns the list of possible accepted keys by an action"
  @spec action_inputs(Ash.Resource.t(), action :: atom()) ::
          MapSet.t()
  def action_inputs(resource, action) do
    Extension.get_persisted(resource, {:action_inputs, action}) || MapSet.new()
  end

  @doc "Returns all attributes of a resource"
  @spec attributes(Spark.Dsl.t() | Ash.Resource.t()) :: [Ash.Resource.Attribute.t()]
  def attributes(resource) do
    Extension.get_entities(resource, [:attributes])
  end

  @doc "Returns all attributes of a resource with lazy non-matching-defaults"
  @spec lazy_non_matching_default_attributes(
          Spark.Dsl.t() | Ash.Resource.t(),
          type :: :create | :update
        ) :: [Ash.Resource.Attribute.t()]
  def lazy_non_matching_default_attributes(resource, :create) do
    Extension.get_persisted(resource, :create_attributes_with_non_matching_lazy_defaults) ||
      Enum.filter(attributes(resource), fn attribute ->
        !attribute.match_other_defaults? &&
          (is_function(attribute.default) or match?({_, _, _}, attribute.default))
      end)
  end

  def lazy_non_matching_default_attributes(resource, :update) do
    Extension.get_persisted(resource, :update_attributes_with_non_matching_lazy_defaults) ||
      Enum.filter(attributes(resource), fn attribute ->
        !attribute.match_other_defaults? &&
          (is_function(attribute.update_default) or match?({_, _, _}, attribute.update_default))
      end)
  end

  @doc "Returns all attributes of a resource with static defaults"
  @spec static_default_attributes(
          Spark.Dsl.t() | Ash.Resource.t(),
          type :: :create | :update
        ) :: [Ash.Resource.Attribute.t()]
  def static_default_attributes(resource, :create) do
    Extension.get_persisted(resource, :create_attributes_with_static_defaults) ||
      resource
      |> attributes
      |> Enum.filter(fn attribute ->
        not is_nil(attribute.default) &&
          not (is_function(attribute.default) or
                 match?({_, _, _}, attribute.default))
      end)
  end

  def static_default_attributes(resource, :update) do
    Extension.get_persisted(resource, :update_attributes_with_static_defaults) ||
      resource
      |> attributes
      |> Enum.filter(fn attribute ->
        not is_nil(attribute.update_default) &&
          not (is_function(attribute.update_default) or
                 match?({_, _, _}, attribute.update_default))
      end)
  end

  @doc "Returns all attributes of a resource with lazy matching defaults"
  @spec lazy_matching_default_attributes(
          Spark.Dsl.t() | Ash.Resource.t(),
          type :: :create | :update
        ) :: [Ash.Resource.Attribute.t()]
  def lazy_matching_default_attributes(resource, :create) do
    Extension.get_persisted(resource, :create_attributes_with_matching_defaults) ||
      Enum.filter(attributes(resource), fn attribute ->
        attribute.match_other_defaults? &&
          (is_function(attribute.default) or match?({_, _, _}, attribute.default))
      end)
  end

  def lazy_matching_default_attributes(resource, :update) do
    Extension.get_persisted(resource, :update_attributes_with_matching_defaults) ||
      resource
      |> attributes()
      |> Enum.filter(fn attribute ->
        not is_nil(attribute.update_default) &&
          not (is_function(attribute.update_default) or
                 match?({_, _, _}, attribute.update_default))
      end)
  end

  @doc "Get an attribute name from the resource"
  @spec attribute(Spark.Dsl.t() | Ash.Resource.t(), String.t() | atom) ::
          Ash.Resource.Attribute.t() | nil
  def attribute(resource, name) when is_binary(name) do
    Extension.get_persisted(resource, :attributes_by_name)[name] ||
      resource
      |> attributes()
      |> Enum.find(&(to_string(&1.name) == name))
  end

  def attribute(resource, name) do
    Extension.get_persisted(resource, :attributes_by_name)[name] ||
      resource
      |> attributes()
      |> Enum.find(&(&1.name == name))
  end

  @doc "Returns all public attributes of a resource"
  @spec public_attributes(Spark.Dsl.t() | Ash.Resource.t()) :: [Ash.Resource.Attribute.t()]
  def public_attributes(resource) do
    resource
    |> attributes()
    |> Enum.filter(& &1.public?)
  end

  @doc "Get a public attribute name from the resource"
  @spec public_attribute(Spark.Dsl.t() | Ash.Resource.t(), String.t() | atom) ::
          Ash.Resource.Attribute.t() | nil
  def public_attribute(resource, name) do
    case attribute(resource, name) do
      %{public?: true} = attr -> attr
      _ -> nil
    end
  end

  @spec related(Spark.Dsl.t() | Ash.Resource.t(), atom() | String.t() | [atom() | String.t()]) ::
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

  @doc "Returns all attributes, aggregates, calculations and relationships of a resource"
  @spec fields(
          Spark.Dsl.t() | Ash.Resource.t(),
          types :: list(:attributes | :aggregates | :calculations | :relationships)
        ) :: [
          Ash.Resource.Attribute.t()
          | Ash.Resource.Aggregate.t()
          | Ash.Resource.Calculation.t()
          | Ash.Resource.Relationships.relationship()
        ]
  def fields(resource, types \\ [:attributes, :aggregates, :calculations, :relationships]) do
    Enum.flat_map(types, &Extension.get_entities(resource, [&1]))
  end

  @doc "Get a field from a resource by name"
  @spec field(Spark.Dsl.t() | Ash.Resource.t(), String.t() | atom) ::
          Ash.Resource.Attribute.t()
          | Ash.Resource.Aggregate.t()
          | Ash.Resource.Calculation.t()
          | Ash.Resource.Relationships.relationship()
          | nil
  def field(resource, name),
    do:
      attribute(resource, name) ||
        aggregate(resource, name) ||
        calculation(resource, name) ||
        relationship(resource, name)

  @doc "Returns all public attributes, aggregates, calculations and relationships of a resource"
  @spec public_fields(Spark.Dsl.t() | Ash.Resource.t()) :: [
          Ash.Resource.Attribute.t()
          | Ash.Resource.Aggregate.t()
          | Ash.Resource.Calculation.t()
          | Ash.Resource.Relationships.relationship()
        ]
  def public_fields(resource) do
    resource
    |> fields()
    |> Enum.filter(& &1.public?)
  end

  @doc "Get a public field from a resource by name"
  @spec public_field(Spark.Dsl.t() | Ash.Resource.t(), String.t() | atom) ::
          Ash.Resource.Attribute.t()
          | Ash.Resource.Aggregate.t()
          | Ash.Resource.Calculation.t()
          | Ash.Resource.Relationships.relationship()
          | nil
  def public_field(resource, name),
    do:
      public_attribute(resource, name) ||
        public_aggregate(resource, name) ||
        public_calculation(resource, name) ||
        public_relationship(resource, name)

  @doc "Determine if a field is sortable by name"
  @spec sortable?(Spark.Dsl.t() | Ash.Resource.t(), String.t() | atom,
          pagination_type: Ash.Page.type(),
          include_private?: boolean()
        ) ::
          boolean()
  def sortable?(resource, name, opts \\ []) do
    pagination_type = Keyword.get(opts, :pagination_type, :offset)
    include_private? = Keyword.get(opts, :include_private?, true)

    field = if include_private?, do: field(resource, name), else: public_field(resource, name)

    case field do
      nil ->
        false

      %{type: {:array, _}} ->
        false

      %{type: Ash.Type.Map} ->
        false

      %Ash.Resource.Relationships.BelongsTo{} ->
        false

      %Ash.Resource.Relationships.HasOne{} ->
        false

      %Ash.Resource.Relationships.HasMany{} ->
        false

      %Ash.Resource.Relationships.ManyToMany{} ->
        false

      %Ash.Resource.Calculation{calculation: {module, _}} ->
        Code.ensure_compiled(module)
        :erlang.function_exported(module, :expression, 2) && pagination_type == :offset

      %Ash.Resource.Calculation{} ->
        false

      %Ash.Resource.Aggregate{kind: :first, relationship_path: relationship_path} = aggregate ->
        related = related(resource, relationship_path)
        sortable?(related, aggregate.field) && pagination_type == :offset

      %Ash.Resource.Aggregate{} ->
        pagination_type == :offset

      _ ->
        true
    end
  end

  @doc "The data layer of the resource, or nil if it does not have one"
  @spec data_layer(Ash.Resource.t()) :: Ash.DataLayer.t() | nil
  defdelegate data_layer(resource), to: Ash.DataLayer
end
