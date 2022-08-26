defmodule Ash.Resource.Info do
  @moduledoc "Introspection for resources"

  alias Spark.Dsl.Extension

  @deprecated "Use `Ash.Resource.set_metadata/2` instead"
  defdelegate set_metadata(record, map), to: Ash.Resource

  @deprecated "Use `Ash.Resource.put_metadata/3` instead"
  defdelegate put_metadata(record, key, term), to: Ash.Resource

  @deprecated "Use `Ash.Resource.unload_many/2` instead"
  defdelegate unload_many(record, loads), to: Ash.Resource

  @deprecated "Use `Ash.Resource.unload/2` instead"
  defdelegate unload(record, key_or_path), to: Ash.Resource

  @deprecated "Use `Ash.Resource.get_metadata/2` instead"
  defdelegate get_metadata(record, key_or_path), to: Ash.Resource

  @deprecated "Use `Ash.Resource.selected?/2` instead"
  defdelegate selected?(record, field), to: Ash.Resource

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
  @spec interfaces(Ash.Resource.t()) :: [Ash.Resource.Interface.t()]
  def interfaces(resource) do
    Extension.get_entities(resource, [:code_interface])
  end

  @doc """
  The Api to define the interface for, when defining it in the resource
  """
  @spec define_interface_for(Ash.Resource.t()) :: atom | nil
  def define_interface_for(resource) do
    Extension.get_opt(resource, [:code_interface], :define_for, nil)
  end

  @doc """
  Whether or not the resource is an embedded resource
  """
  @spec embedded?(Ash.Resource.t()) :: boolean
  def embedded?(resource) do
    Extension.get_persisted(resource, :embedded?, false)
  end

  @doc """
  The description of the resource
  """
  @spec description(Ash.Resource.t()) :: String.t() | nil
  def description(resource) do
    Extension.get_opt(resource, [:resource], :description, "no description")
  end

  @doc """
  The trace_name of the resource
  """
  @spec trace_name(Ash.Resource.t()) :: String.t() | nil
  def trace_name(resource) do
    Extension.get_opt(resource, [:resource], :trace_name, nil) || to_string(short_name(resource))
    resource |> Module.split() |> List.last()
  end

  @doc """
  The short_name of the resource
  """
  @spec short_name(Ash.Resource.t()) :: String.t() | nil
  def short_name(resource) do
    Extension.get_opt(resource, [:resource], :short_name, nil) || resource.default_short_name()
  end

  @doc """
  The base filter of the resource
  """
  @spec base_filter(Ash.Resource.t()) :: term
  def base_filter(resource) do
    Extension.get_opt(resource, [:resource], :base_filter, nil)
  end

  @doc """
  The default context of the resource
  """
  @spec default_context(Ash.Resource.t()) :: term
  def default_context(resource) do
    Extension.get_opt(resource, [:resource], :default_context, nil)
  end

  @doc "A list of identities for the resource"
  @spec identities(Ash.Resource.t()) :: [Ash.Resource.Identity.t()]
  def identities(resource) do
    Extension.get_entities(resource, [:identities])
  end

  @doc "Get an identity by name from the resource"
  @spec identity(Ash.Resource.t(), atom) :: Ash.Resource.Identity.t() | nil
  def identity(resource, name) do
    resource
    |> identities()
    |> Enum.find(&(&1.name == name))
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

  @doc "A list of all validations for the resource for a given action type"
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

  @doc "A list of all changes for the resource for a given action type"
  @spec changes(Ash.Resource.t(), :create | :update | :destroy) ::
          list(
            Ash.Resource.Validation.t()
            | Ash.Resource.Change.t()
          )
  def changes(resource, type) do
    resource
    |> changes()
    |> Enum.filter(&(type in &1.on))
  end

  @doc "A list of all changes for the resource"
  @spec changes(Ash.Resource.t()) :: list(Ash.Resource.Validation.t() | Ash.Resource.Change.t())
  def changes(resource) do
    Extension.get_entities(resource, [:changes])
  end

  @spec preparations(Ash.Resource.t()) :: list(Ash.Resource.Preparation.t())
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
  @spec primary_key(Ash.Resource.t()) :: list(atom)
  def primary_key(resource) do
    Spark.Dsl.Extension.get_persisted(resource, :primary_key, [])
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

  @doc "The multitenancy strategy for a resource"
  @spec multitenancy_strategy(Ash.Resource.t()) :: :context | :attribute | nil
  def multitenancy_strategy(resource) do
    Spark.Dsl.Extension.get_opt(resource, [:multitenancy], :strategy, nil)
  end

  @doc "The multitenancy attribute for a resource"
  @spec multitenancy_attribute(Ash.Resource.t()) :: atom | nil
  def multitenancy_attribute(resource) do
    Spark.Dsl.Extension.get_opt(resource, [:multitenancy], :attribute, nil)
  end

  @doc "The function to parse the tenant from the attribute"
  @spec multitenancy_parse_attribute(Ash.Resource.t()) :: {atom, atom, list(any)}
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
  @spec multitenancy_global?(Ash.Resource.t()) :: atom | nil
  def multitenancy_global?(resource) do
    Spark.Dsl.Extension.get_opt(resource, [:multitenancy], :global?, nil)
  end

  @doc "The source attribute for multitenancy"
  @spec multitenancy_source(Ash.Resource.t()) :: atom | nil
  def multitenancy_source(resource) do
    Spark.Dsl.Extension.get_opt(resource, [:multitenancy], :source, nil)
  end

  @doc "The template for creating the tenant name"
  @spec multitenancy_template(Ash.Resource.t()) :: atom | nil
  def multitenancy_template(resource) do
    Spark.Dsl.Extension.get_opt(resource, [:multitenancy], :template, nil)
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
      nil -> raise "Required primary #{type} action for #{inspect(resource)}."
      action -> action
    end
  end

  @doc "Returns the primary action of a given type"
  @spec primary_action(Ash.Resource.t(), Ash.Resource.Actions.action_type()) ::
          Ash.Resource.Actions.action() | nil
  def primary_action(resource, type) do
    resource
    |> actions()
    |> Enum.find(&(&1.type == type && &1.primary?))
  end

  @doc "Returns the configured default actions"
  @spec default_actions(Ash.Resource.t()) :: list(:create | :read | :update | :destroy)
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
  @spec actions(Ash.Resource.t()) :: [Ash.Resource.Actions.action()]
  def actions(resource) do
    Extension.get_entities(resource, [:actions])
  end

  @doc "Returns the action with the matching name and type on the resource"
  @spec action(Ash.Resource.t(), atom(), Ash.Resource.Actions.action_type() | nil) ::
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

          Perhaps you passed a changeset with the incorrect action type into your Api?
          """
      end
    else
      resource
      |> actions()
      |> Enum.find(&(&1.name == name))
    end
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

  @doc "Get a field from a resource by name"
  @spec field(Ash.Resource.t(), String.t() | atom) ::
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

  @doc "Get a public field from a resource by name"
  @spec public_field(Ash.Resource.t(), String.t() | atom) ::
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
  @spec sortable?(Ash.Resource.t(), String.t() | atom,
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
end
