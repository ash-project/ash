defmodule Ash.Changeset do
  @moduledoc """
  Changesets are used to create and update data in Ash.

  Create a changeset with `new/1` or `new/2`, and alter the attributes
  and relationships using the functions provided in this module.  Nothing in this module
  actually incurs changes in a data layer. To commit a changeset, see `c:Ash.Api.create/2`
  and `c:Ash.Api.update/2`.

  ## Primary Keys

  For relationship manipulation using `append_to_relationship/3`, `remove_from_relationship/3`
  and `replace_relationship/3` there are three types that can be used for primary keys:

  1.) An instance of the resource in question.

  2.) If the primary key is just a single field, i.e `:id`, then a single value, i.e `1`

  3.) A map of keys to values representing the primary key, i.e `%{id: 1}` or `%{id: 1, org_id: 2}`

  ## Join Attributes

  For many to many relationships, the attributes on a join relationship may be set while relating items
  by passing a tuple of the primary key and the changes to be applied. This is done via upserts, so
  update validations on the join resource are *not* applied, but create validations are.

  For example:

  ```elixir
  Ash.Changeset.replace_relationship(changeset, :linked_tickets, [
    {1, %{link_type: "blocking"}},
    {a_ticket, %{link_type: "caused_by"}},
    {%{id: 2}, %{link_type: "related_to"}}
  ])
  ```
  """
  defstruct [
    :data,
    :action_type,
    :resource,
    :api,
    :tenant,
    arguments: %{},
    context: %{},
    after_action: [],
    before_action: [],
    errors: [],
    valid?: true,
    attributes: %{},
    relationships: %{},
    change_dependencies: [],
    requests: []
  ]

  defimpl Inspect do
    import Inspect.Algebra

    def inspect(changeset, opts) do
      container_doc(
        "#Ash.Changeset<",
        [
          concat("action_type: ", inspect(changeset.action_type)),
          concat("attributes: ", to_doc(changeset.attributes, opts)),
          concat("relationships: ", to_doc(changeset.relationships, opts)),
          concat("errors: ", to_doc(changeset.errors, opts)),
          concat("data: ", to_doc(changeset.data, opts)),
          concat("valid?: ", to_doc(changeset.valid?, opts))
        ],
        ">",
        opts,
        fn str, _ -> str end
      )
    end
  end

  @type t :: %__MODULE__{}

  alias Ash.Error.{
    Changes.InvalidArgument,
    Changes.InvalidAttribute,
    Changes.InvalidRelationship,
    Changes.NoSuchAttribute,
    Changes.NoSuchRelationship,
    Changes.Required,
    Invalid.NoSuchResource,
    Query.NoReadAction
  }

  @doc "Return a changeset over a resource or a record"
  @spec new(Ash.resource() | Ash.record(), initial_attributes :: map) :: t
  def new(resource, initial_attributes \\ %{})

  def new(%resource{} = record, initial_attributes) do
    tenant =
      record
      |> Map.get(:__metadata__, %{})
      |> Map.get(:tenant, nil)

    if Ash.Resource.resource?(resource) do
      %__MODULE__{resource: resource, data: record, action_type: :update}
      |> change_attributes(initial_attributes)
      |> set_tenant(tenant)
    else
      %__MODULE__{resource: resource, action_type: :create, data: struct(resource)}
      |> add_error(NoSuchResource.exception(resource: resource))
      |> set_tenant(tenant)
    end
  end

  def new(resource, initial_attributes) do
    if Ash.Resource.resource?(resource) do
      %__MODULE__{resource: resource, action_type: :create, data: struct(resource)}
      |> change_attributes(initial_attributes)
    else
      %__MODULE__{resource: resource, action_type: :create, data: struct(resource)}
      |> add_error(NoSuchResource.exception(resource: resource))
    end
  end

  @doc """
  Wraps a function in the before/after action hooks of a changeset.

  The function takes a changeset and if it returns
  `{:ok, result}`, the result will be passed through the after
  action hooks.
  """
  @spec with_hooks(
          t(),
          (t() ->
             {:ok, Ash.record(), %{notifications: list(Ash.notification())}} | {:error, term})
        ) ::
          {:ok, term} | {:error, term}
  def with_hooks(changeset, func) do
    changeset =
      Enum.reduce_while(changeset.before_action, changeset, fn before_action, changeset ->
        case before_action.(changeset) do
          %{valid?: true} = changeset -> {:cont, changeset}
          changeset -> {:halt, changeset}
        end
      end)

    if changeset.valid? do
      case func.(changeset) do
        {:ok, result} ->
          Enum.reduce_while(
            changeset.after_action,
            {:ok, result, %{notifications: []}},
            fn after_action, {:ok, result, %{notifications: notifications} = acc} ->
              case after_action.(changeset, result) do
                {:ok, new_result, new_notifications} ->
                  {:cont,
                   {:ok, new_result, %{acc | notifications: notifications ++ new_notifications}}}

                {:ok, new_result} ->
                  {:cont, {:ok, new_result, acc}}

                {:error, error} ->
                  {:halt, {:error, error}}
              end
            end
          )

        {:error, error} ->
          {:error, error}
      end
    else
      {:error, changeset.errors}
    end
  end

  @doc "Gets the value of an argument provided to the changeset"
  @spec get_argument(t, atom) :: term
  def get_argument(changeset, argument) when is_atom(argument) do
    Map.get(changeset.arguments, argument) || Map.get(changeset.arguments, to_string(argument))
  end

  @doc "Gets the changing value or the original value of an attribute"
  @spec get_attribute(t, atom) :: term
  def get_attribute(changeset, attribute) do
    case fetch_change(changeset, attribute) do
      {:ok, value} ->
        value

      :error ->
        get_data(changeset, attribute)
    end
  end

  @doc "Gets the new value for an attribute, or `:error` if it is not being changed"
  @spec fetch_change(t, atom) :: {:ok, any} | :error
  def fetch_change(changeset, attribute) do
    Map.fetch(changeset.attributes, attribute)
  end

  @doc "Gets the original value for an attribute"
  @spec get_data(t, atom) :: {:ok, any} | :error
  def get_data(changeset, attribute) do
    Map.get(changeset.data, attribute)
  end

  @spec put_context(t(), atom, term) :: t()
  def put_context(changeset, key, value) do
    %{changeset | context: Map.put(changeset.context, key, value)}
  end

  @spec set_tenant(t(), String.t()) :: t()
  def set_tenant(changeset, tenant) do
    %{changeset | tenant: tenant}
  end

  @spec set_context(t(), map) :: t()
  def set_context(changeset, map) do
    %{changeset | context: Map.merge(changeset.context, map)}
  end

  @doc false
  def cast_arguments(changeset, action) do
    Enum.reduce(action.arguments, %{changeset | arguments: %{}}, fn argument, new_changeset ->
      value = get_argument(changeset, argument.name)

      if is_nil(value) && !argument.allow_nil? do
        Ash.Changeset.add_error(
          changeset,
          Required.exception(field: argument.name, type: :argument)
        )
      else
        with {:ok, casted} <- Ash.Type.cast_input(argument.type, value),
             :ok <- Ash.Type.apply_constraints(argument.type, casted, argument.constraints) do
          %{new_changeset | arguments: Map.put(new_changeset.arguments, argument.name, value)}
        else
          _ ->
            Ash.Changeset.add_error(
              changeset,
              InvalidArgument.exception(field: argument.name)
            )
        end
      end
    end)
  end

  @doc """
  Appends a record or a list of records to a relationship. Stacks with previous removals/additions.

  Accepts a primary key or a list of primary keys. See the section on "Primary Keys" in the
  module documentation for more.

  For many to many relationships, accepts changes for any `join_attributes` configured on
  the resource. See the section on "Join Attributes" in the module documentation for more.

  Cannot be used with `belongs_to` or `has_one` relationships.
  See `replace_relationship/3` for manipulating `belongs_to` and `has_one` relationships.
  """
  @spec append_to_relationship(t, atom, Ash.primary_key() | [Ash.primary_key()]) :: t()
  def append_to_relationship(changeset, relationship, record_or_records) do
    case Ash.Resource.relationship(changeset.resource, relationship) do
      nil ->
        error =
          NoSuchRelationship.exception(
            resource: changeset.resource,
            name: relationship
          )

        add_error(changeset, error)

      %{cardinality: :one, type: type} = relationship ->
        error =
          InvalidRelationship.exception(
            relationship: relationship.name,
            message: "Cannot append to a #{type} relationship"
          )

        add_error(changeset, error)

      %{writable?: false} = relationship ->
        error =
          InvalidRelationship.exception(
            relationship: relationship.name,
            message: "Relationship is not editable"
          )

        add_error(changeset, error)

      %{type: :many_to_many} = relationship ->
        case primary_keys_with_changes(relationship, List.wrap(record_or_records)) do
          {:ok, primary_keys} ->
            relationships =
              changeset.relationships
              |> Map.put_new(relationship.name, %{})
              |> add_to_relationship_key_and_reconcile(relationship, :add, primary_keys)

            %{changeset | relationships: relationships}

          {:error, error} ->
            add_error(changeset, error)
        end

      relationship ->
        case primary_key(relationship, List.wrap(record_or_records)) do
          {:ok, primary_keys} ->
            relationships =
              changeset.relationships
              |> Map.put_new(relationship.name, %{})
              |> add_to_relationship_key_and_reconcile(relationship, :add, primary_keys)

            %{changeset | relationships: relationships}

          {:error, error} ->
            add_error(changeset, error)
        end
    end
  end

  @doc """
  Removes a record or a list of records to a relationship. Stacks with previous removals/additions.

  Accepts a primary key or a list of primary keys. See the section on "Primary Keys" in the
  module documentation for more.

  Cannot be used with `belongs_to` or `has_one` relationships.
  See `replace_relationship/3` for manipulating those relationships.
  """
  @spec remove_from_relationship(t, atom, Ash.primary_key() | [Ash.primary_key()]) :: t()
  def remove_from_relationship(changeset, relationship, record_or_records) do
    case Ash.Resource.relationship(changeset.resource, relationship) do
      nil ->
        error =
          NoSuchRelationship.exception(
            resource: changeset.resource,
            name: relationship
          )

        add_error(changeset, error)

      %{cardinality: :one, type: type} = relationship ->
        error =
          InvalidRelationship.exception(
            relationship: relationship.name,
            message: "Cannot remove from a #{type} relationship"
          )

        add_error(changeset, error)

      %{writable?: false} = relationship ->
        error =
          InvalidRelationship.exception(
            relationship: relationship.name,
            message: "Relationship is not editable"
          )

        add_error(changeset, error)

      relationship ->
        case primary_key(relationship, List.wrap(record_or_records)) do
          {:ok, primary_keys} ->
            relationships =
              changeset.relationships
              |> Map.put_new(relationship.name, %{})
              |> add_to_relationship_key_and_reconcile(relationship, :remove, primary_keys)

            %{changeset | relationships: relationships}

          {:error, error} ->
            add_error(changeset, error)
        end
    end
  end

  defp add_to_relationship_key_and_reconcile(relationships, relationship, key, to_add) do
    Map.update!(relationships, relationship.name, fn relationship_changes ->
      relationship_changes
      |> Map.put_new(key, [])
      |> Map.update!(key, &Kernel.++(to_add, &1))
      |> reconcile_relationship_changes()
    end)
  end

  @doc """
  Replaces the value of a relationship. Any previous additions/removals are cleared.

  Accepts a primary key or a list of primary keys. See the section on "Primary Keys" in the
  module documentation for more.

  For many to many relationships, accepts changes for any `join_attributes` configured on
  the resource. See the section on "Join Attributes" in the module documentation for more.

  For a `has_many` or `many_to_many` relationship, this means removing any currently related
  records that are not present in the replacement list, and creating any that do not exist
  in the data layer.

  For a `belongs_to` or `has_one`, replace with a `nil` value to unset a relationship.
  """
  @spec replace_relationship(
          t(),
          atom(),
          Ash.primary_key() | [Ash.primary_key()] | nil
        ) :: t()
  def replace_relationship(changeset, relationship, record_or_records) do
    case Ash.Resource.relationship(changeset.resource, relationship) do
      nil ->
        error =
          NoSuchRelationship.exception(
            resource: changeset.resource,
            name: relationship
          )

        add_error(changeset, error)

      %{writable?: false} = relationship ->
        error =
          InvalidRelationship.exception(
            relationship: relationship.name,
            message: "Relationship is not editable"
          )

        add_error(changeset, error)

      %{cardinality: :one, type: type}
      when is_list(record_or_records) and length(record_or_records) > 1 ->
        error =
          InvalidRelationship.exception(
            relationship: relationship.name,
            message: "Cannot replace a #{type} relationship with multiple records"
          )

        add_error(changeset, error)

      %{type: :many_to_many} = relationship ->
        do_replace_many_to_many_relationship(changeset, relationship, record_or_records)

      relationship ->
        if Ash.Resource.primary_action(relationship.destination, :read) do
          records =
            if relationship.cardinality == :one do
              if is_list(record_or_records) do
                List.first(record_or_records)
              else
                record_or_records
              end
            else
              List.wrap(record_or_records)
            end

          case primary_key(relationship, records) do
            {:ok, primary_key} ->
              relationships =
                Map.put(changeset.relationships, relationship.name, %{replace: primary_key})

              changeset
              |> check_entities_for_direct_write(relationship.name, List.wrap(records))
              |> Map.put(:relationships, relationships)

            {:error, error} ->
              add_error(changeset, error)
          end
        else
          add_error(
            changeset,
            NoReadAction.exception(
              resource: changeset.resource,
              when: "replacing relationship #{relationship.name}"
            )
          )
        end
    end
  end

  defp do_replace_many_to_many_relationship(changeset, relationship, record_or_records) do
    cond do
      !Ash.Resource.primary_action(relationship.destination, :read) ->
        add_error(
          changeset,
          NoReadAction.exception(
            resource: changeset.resource,
            when: "replacing relationship #{relationship.name}"
          )
        )

      !Ash.Resource.primary_action(relationship.through, :read) ->
        add_error(
          changeset,
          NoReadAction.exception(
            resource: changeset.resource,
            when: "replacing relationship #{relationship.name}"
          )
        )

      true ->
        case primary_keys_with_changes(relationship, List.wrap(record_or_records)) do
          {:ok, primary_key} ->
            relationships =
              Map.put(changeset.relationships, relationship.name, %{replace: primary_key})

            %{changeset | relationships: relationships}

          {:error, error} ->
            add_error(changeset, error)
        end
    end
  end

  defp check_entities_for_direct_write(changeset, relationship_name, records) do
    if Enum.all?(records, &is_resource?/1) do
      relation_entities =
        Map.merge(Map.get(changeset.context, :destination_entities, %{}), %{
          relationship_name => Enum.group_by(records, & &1.__struct__)
        })

      put_context(changeset, :destination_entities, relation_entities)
    else
      if Ash.Resource.primary_action(
           Ash.Resource.related(changeset.resource, relationship_name),
           :read
         ) do
        changeset
      else
        add_error(
          changeset,
          NoReadAction.exception(
            resource: changeset.resource,
            when: "editing relationship #{relationship_name} and not supplying full records"
          )
        )
      end
    end
  end

  defp is_resource?(record) do
    Ash.Resource.resource?(record.__struct__)
  rescue
    _error ->
      false
  end

  @doc "Returns true if an attribute exists in the changes"
  @spec changing_attribute?(t(), atom) :: boolean
  def changing_attribute?(changeset, attribute) do
    Map.has_key?(changeset.attributes, attribute)
  end

  @doc "Returns true if a relationship exists in the changes"
  @spec changing_relationship?(t(), atom) :: boolean
  def changing_relationship?(changeset, relationship) do
    Map.has_key?(changeset.relationships, relationship)
  end

  @doc "Change an attribute only if is not currently being changed"
  @spec change_new_attribute(t(), atom, term) :: t()
  def change_new_attribute(changeset, attribute, value) do
    if changing_attribute?(changeset, attribute) do
      changeset
    else
      change_attribute(changeset, attribute, value)
    end
  end

  @doc """
  Change an attribute if is not currently being changed, by calling the provided function

  Use this if you want to only perform some expensive calculation for an attribute value
  only if there isn't already a change for that attribute
  """
  @spec change_new_attribute_lazy(t(), atom, (() -> any)) :: t()
  def change_new_attribute_lazy(changeset, attribute, func) do
    if changing_attribute?(changeset, attribute) do
      changeset
    else
      change_attribute(changeset, attribute, func.())
    end
  end

  @doc """
  Add an argument to the changeset, which will be provided to the action
  """
  def set_argument(changeset, argument, value) do
    %{changeset | arguments: Map.put(changeset.arguments, argument, value)}
  end

  @doc """
  Remove an argument from the changeset
  """
  def delete_argument(changeset, argument_or_arguments) do
    argument_or_arguments
    |> List.wrap()
    |> Enum.reduce(changeset, fn argument, changeset ->
      %{changeset | arguments: Map.delete(changeset.arguments, argument)}
    end)
  end

  @doc """
  Merge a map of arguments to the arguments list
  """
  def set_arguments(changeset, map) do
    %{changeset | arguments: Map.merge(changeset.arguments, map)}
  end

  @doc """
  Force change an attribute if is not currently being changed, by calling the provided function

  See `change_new_attribute_lazy/3` for more.
  """
  @spec force_change_new_attribute_lazy(t(), atom, (() -> any)) :: t()
  def force_change_new_attribute_lazy(changeset, attribute, func) do
    if changing_attribute?(changeset, attribute) do
      changeset
    else
      force_change_attribute(changeset, attribute, func.())
    end
  end

  @doc "Calls `change_attribute/3` for each key/value pair provided"
  @spec change_attributes(t(), map | Keyword.t()) :: t()
  def change_attributes(changeset, changes) do
    Enum.reduce(changes, changeset, fn {key, value}, changeset ->
      change_attribute(changeset, key, value)
    end)
  end

  @doc "Adds a change to the changeset, unless the value matches the existing value"
  def change_attribute(changeset, attribute, value) do
    case Ash.Resource.attribute(changeset.resource, attribute) do
      nil ->
        error =
          NoSuchAttribute.exception(
            resource: changeset.resource,
            name: attribute
          )

        add_error(changeset, error)

      %{writable?: false} = attribute ->
        add_attribute_invalid_error(changeset, attribute, "Attribute is not writable")

      attribute ->
        with {:ok, casted} <- Ash.Type.cast_input(attribute.type, value),
             :ok <- validate_allow_nil(attribute, casted),
             :ok <- Ash.Type.apply_constraints(attribute.type, casted, attribute.constraints) do
          data_value = Map.get(changeset.data, attribute.name)

          cond do
            is_nil(data_value) and is_nil(casted) ->
              changeset

            Ash.Type.equal?(attribute.type, casted, data_value) ->
              changeset

            true ->
              %{changeset | attributes: Map.put(changeset.attributes, attribute.name, casted)}
          end
        else
          :error ->
            add_attribute_invalid_error(changeset, attribute)

          {:error, error_or_errors} ->
            error_or_errors
            |> List.wrap()
            |> Enum.reduce(changeset, &add_attribute_invalid_error(&2, attribute, &1))
        end
    end
  end

  @doc "Calls `force_change_attribute/3` for each key/value pair provided"
  @spec force_change_attributes(t(), map) :: t()
  def force_change_attributes(changeset, changes) do
    Enum.reduce(changes, changeset, fn {key, value}, changeset ->
      force_change_attribute(changeset, key, value)
    end)
  end

  @doc "Changes an attribute even if it isn't writable"
  @spec force_change_attribute(t(), atom, any) :: t()
  def force_change_attribute(changeset, attribute, value) do
    case Ash.Resource.attribute(changeset.resource, attribute) do
      nil ->
        error =
          NoSuchAttribute.exception(
            resource: changeset.resource,
            name: attribute
          )

        add_error(changeset, error)

      attribute when is_nil(value) ->
        %{changeset | attributes: Map.put(changeset.attributes, attribute.name, nil)}

      attribute ->
        with {:ok, casted} <- Ash.Type.cast_input(attribute.type, value),
             :ok <- Ash.Type.apply_constraints(attribute.type, casted, attribute.constraints) do
          data_value = Map.get(changeset.data, attribute.name)

          cond do
            is_nil(data_value) and is_nil(casted) ->
              changeset

            Ash.Type.equal?(attribute.type, casted, data_value) ->
              changeset

            true ->
              %{changeset | attributes: Map.put(changeset.attributes, attribute.name, casted)}
          end
        else
          :error ->
            add_attribute_invalid_error(changeset, attribute)

          {:error, error_or_errors} ->
            error_or_errors
            |> List.wrap()
            |> Enum.reduce(changeset, &add_attribute_invalid_error(&2, attribute, &1))
        end
    end
  end

  @doc "Adds a before_action hook to the changeset."
  @spec before_action(t(), (t() -> t())) :: t()
  def before_action(changeset, func) do
    %{changeset | before_action: [func | changeset.before_action]}
  end

  @doc "Adds an after_action hook to the changeset."
  @spec after_action(
          t(),
          (t(), Ash.record() ->
             {:ok, Ash.record()} | {:ok, Ash.record(), list(Ash.notification())} | {:error, term})
        ) :: t()
  def after_action(changeset, func) do
    %{changeset | after_action: [func | changeset.after_action]}
  end

  @doc "Returns the original data with attribute changes merged."
  @spec apply_attributes(t()) :: Ash.record()
  def apply_attributes(changeset) do
    Enum.reduce(changeset.attributes, changeset.data, fn {attribute, value}, data ->
      Map.put(data, attribute, value)
    end)
  end

  @doc "Adds an error to the changesets errors list, and marks the change as `valid?: false`"
  @spec add_error(t(), Ash.error()) :: t()
  def add_error(changeset, error) do
    %{changeset | errors: [error | changeset.errors], valid?: false}
  end

  defp reconcile_relationship_changes(%{replace: _, add: add} = changes) do
    changes
    |> Map.delete(:add)
    |> Map.update!(:replace, fn replace ->
      replace ++ add
    end)
    |> reconcile_relationship_changes()
  end

  defp reconcile_relationship_changes(%{replace: _, remove: remove} = changes) do
    changes
    |> Map.delete(:remove)
    |> Map.update!(:replace, fn replace ->
      Enum.reject(replace, &(&1 in remove))
    end)
    |> reconcile_relationship_changes()
  end

  defp reconcile_relationship_changes(changes) do
    changes
    |> update_if_present(:replace, &uniq_if_list/1)
    |> update_if_present(:remove, &uniq_if_list/1)
    |> update_if_present(:add, &uniq_if_list/1)
  end

  defp uniq_if_list(list) when is_list(list), do: Enum.uniq(list)
  defp uniq_if_list(other), do: other

  defp update_if_present(map, key, func) do
    if Map.has_key?(map, key) do
      Map.update!(map, key, func)
    else
      map
    end
  end

  defp through_changeset(relationship, changes) do
    new(relationship.through, changes)
  end

  defp primary_keys_with_changes(_, []), do: {:ok, []}

  defp primary_keys_with_changes(relationship, records) do
    Enum.reduce_while(records, {:ok, []}, fn
      {record, changes}, {:ok, acc} ->
        with {:ok, primary_key} <- primary_key(relationship, record),
             %{valid?: true} = changeset <- through_changeset(relationship, changes) do
          {:cont, {:ok, [{primary_key, changeset} | acc]}}
        else
          %{valid?: false, errors: errors} -> {:halt, {:error, errors}}
          {:error, error} -> {:halt, {:error, error}}
        end

      record, {:ok, acc} ->
        case primary_key(relationship, record) do
          {:ok, primary_keys} when is_list(primary_keys) ->
            {:cont, {:ok, primary_keys ++ acc}}

          {:ok, primary_key} ->
            {:cont, {:ok, [primary_key | acc]}}

          {:error, error} ->
            {:halt, {:error, error}}
        end
    end)
  end

  defp primary_key(_, nil), do: {:ok, nil}

  defp primary_key(relationship, records) when is_list(records) do
    case Ash.Resource.primary_key(relationship.destination) do
      [_field] ->
        multiple_primary_keys(relationship, records)

      _ ->
        pluck_pk_fields(relationship, records)
    end
  end

  defp primary_key(relationship, record) do
    do_primary_key(relationship, record)
  end

  defp pluck_pk_fields(relationship, records) do
    Enum.reduce_while(
      records,
      {:ok, []},
      fn
        record, {:ok, acc} ->
          case do_primary_key(relationship, record) do
            {:ok, pk} -> {:cont, {:ok, [pk | acc]}}
            {:error, error} -> {:halt, {:error, error}}
          end
      end
    )
  end

  defp do_primary_key(relationship, record) when is_map(record) do
    primary_key = Ash.Resource.primary_key(relationship.destination)

    is_pkey_map? =
      Enum.all?(
        primary_key,
        fn key ->
          Map.has_key?(record, key) || Map.has_key?(record, to_string(key))
        end
      )

    if is_pkey_map? do
      pkey =
        Enum.reduce(primary_key, %{}, fn key, acc ->
          case Map.fetch(record, key) do
            {:ok, value} -> Map.put(acc, key, value)
            :error -> Map.put(acc, key, Map.get(record, to_string(key)))
          end
        end)

      {:ok, pkey}
    else
      error =
        InvalidRelationship.exception(
          relationship: relationship.name,
          message: "Invalid identifier #{inspect(record)}"
        )

      {:error, error}
    end
  end

  defp do_primary_key(relationship, record) do
    single_primary_key(relationship, record)
  end

  defp multiple_primary_keys(relationship, values) do
    Enum.reduce_while(values, {:ok, []}, fn record, {:ok, primary_keys} ->
      case do_primary_key(relationship, record) do
        {:ok, pkey} -> {:cont, {:ok, [pkey | primary_keys]}}
        {:error, error} -> {:halt, {:error, error}}
      end
    end)
  end

  defp single_primary_key(relationship, value) do
    with [field] <- Ash.Resource.primary_key(relationship.destination),
         attribute <- Ash.Resource.attribute(relationship.destination, field),
         {:ok, casted} <- Ash.Type.cast_input(attribute.type, value) do
      {:ok, %{field => casted}}
    else
      _ ->
        error =
          InvalidRelationship.exception(
            relationship: relationship.name,
            message: "Invalid identifier #{inspect(value)}"
          )

        {:error, error}
    end
  end

  @doc false
  def changes_depend_on(changeset, dependency) do
    %{changeset | change_dependencies: [dependency | changeset.change_dependencies]}
  end

  @doc false
  def add_requests(changeset, requests) when is_list(requests) do
    Enum.reduce(requests, changeset, &add_requests(&2, &1))
  end

  def add_requests(changeset, request) do
    %{changeset | requests: [request | changeset.requests]}
  end

  defp validate_allow_nil(%{allow_nil?: false} = attribute, nil) do
    {:error,
     InvalidAttribute.exception(
       field: attribute.name,
       message: "must be present"
     )}
  end

  defp validate_allow_nil(_, _), do: :ok

  defp add_attribute_invalid_error(changeset, attribute, message \\ nil) do
    error =
      InvalidAttribute.exception(
        field: attribute.name,
        message: message
      )

    add_error(changeset, error)
  end
end
