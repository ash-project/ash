defmodule Ash.Actions.Relationships do
  @moduledoc false
  alias Ash.Actions.PrimaryKeyHelpers
  alias Ash.Engine.Request

  def validate_not_changing_relationship_and_source_field(relationships, attributes, resource) do
    resource
    |> Ash.relationships()
    |> Enum.reduce_while({:ok, relationships}, fn relationship, {:ok, relationships} ->
      with {:ok, _} <- fetch_string_or_atom(attributes, relationship.source_field),
           {:ok, _} <- fetch_string_or_atom(relationships, relationship.name) do
        {:halt,
         {:error,
          "Providing data for relationship #{relationship.name} and changing its source field"}}
      else
        _ ->
          {:cont, {:ok, relationships}}
      end
    end)
  end

  def field_changes_into_relationship_changes(
        relationships,
        attributes,
        resource
      ) do
    resource
    |> Ash.relationships()
    |> Enum.reduce_while({:ok, attributes, relationships}, fn
      %{
        type: :belongs_to,
        source_field: source_field,
        name: name,
        destination_field: destination_field
      },
      {:ok, attr_acc, relationships} ->
        case fetch_string_or_atom(attributes, source_field) do
          {:ok, value} ->
            new_attr_acc =
              attr_acc
              |> Map.delete(source_field)
              |> Map.delete(to_string(source_field))

            {:cont,
             {:ok, new_attr_acc, Map.put(relationships, name, [{destination_field, value}])}}

          :error ->
            {:cont, {:ok, attr_acc, relationships}}
        end

      _, {:ok, attr_acc, relationships} ->
        {:cont, {:ok, attr_acc, relationships}}
    end)
  end

  def handle_relationship_changes(changeset, api, relationships_input, action_type) do
    Enum.reduce(relationships_input, changeset, fn {name, data}, changeset ->
      case Ash.relationship(changeset.data.__struct__, name) do
        nil ->
          Ecto.Changeset.add_error(changeset, name, "Invalid relationship")

        relationship ->
          do_handle_relationship_changes(api, changeset, relationship, data, action_type)
      end
    end)
  end

  defp do_handle_relationship_changes(api, changeset, relationship, data, action_type) do
    case validate_relationship_change(relationship, data, action_type) do
      {:ok, input} ->
        add_relationship_read_requests(changeset, api, relationship, input, action_type)

      {:error, error} ->
        {:error, error}
    end
  end

  defp add_relationship_read_requests(changeset, api, relationship, input, :update) do
    changeset
    |> add_replace_requests(api, relationship, input)
    |> add_remove_requests(api, relationship, input)
    |> add_add_requests(api, relationship, input)
  end

  defp add_relationship_read_requests(changeset, api, relationship, input, :create) do
    input =
      case Map.fetch(input, :replace) do
        {:ok, replacing} ->
          Map.update(input, :add, replacing, &Kernel.++(&1, replacing))

        _ ->
          input
      end

    add_add_requests(changeset, api, relationship, input)
  end

  defp add_add_requests(changeset, api, relationship, input) do
    case Map.fetch(input, :add) do
      {:ok, identifiers} ->
        changeset =
          case relationship do
            %{type: :belongs_to, source_field: source_field, destination_field: destination_field} ->
              add_belongs_to_change(changeset, identifiers, source_field, destination_field)

            _ ->
              changeset
          end

        do_add_relationship_read_requests(changeset, api, relationship, identifiers, :add)

      :error ->
        changeset
    end
  end

  defp add_belongs_to_change(changeset, identifiers, source_field, destination_field) do
    case Keyword.fetch(identifiers, destination_field) do
      {:ok, field_value} ->
        Ecto.Changeset.put_change(changeset, source_field, field_value)

      _ ->
        changeset
    end
  end

  defp add_replace_requests(changeset, api, relationship, input) do
    case Map.fetch(input, :replace) do
      {:ok, identifiers} ->
        changeset
        |> do_add_relationship_read_requests(api, relationship, identifiers, :replace)
        |> add_relationship_currently_related_request(api, relationship)

      :error ->
        changeset
    end
  end

  defp add_remove_requests(changeset, api, relationship, input) do
    case Map.fetch(input, :remove) do
      {:ok, identifiers} ->
        changeset =
          case relationship do
            %{type: :belongs_to, source_field: source_field} ->
              Ecto.Changeset.put_change(changeset, source_field, nil)

            _ ->
              changeset
          end

        do_add_relationship_read_requests(
          changeset,
          api,
          relationship,
          identifiers,
          :remove
        )

      :error ->
        changeset
    end
  end

  defp do_add_relationship_read_requests(
         changeset,
         api,
         %{destination: destination} = relationship,
         identifiers,
         type
       ) do
    relationship_name = relationship.name

    filter =
      case identifiers do
        [single_identifier] ->
          if Keyword.keyword?(single_identifier) do
            single_identifier
          else
            [single_identifier]
          end

        [] ->
          [__impossible__: true]

        many ->
          [or: many]
      end

    query =
      api
      |> Ash.Query.new(destination)
      |> Ash.Query.filter(filter)

    request =
      Request.new(
        api: api,
        resource: relationship.destination,
        action: Ash.primary_action!(relationship.destination, :read),
        query: query,
        path: [:relationships, relationship_name, type],
        data:
          Request.resolve(fn _data ->
            query
            |> api.read()
          end),
        name: "read prior to write related #{relationship.name}"
      )

    changeset
    |> add_requests(request)
    |> changes_depend_on([:relationships, relationship_name, type, :data])
  end

  defp validate_relationship_change(relationship, data, action_type) do
    keyword? = Keyword.keyword?(data)

    cond do
      relationship.cardinality == :many && keyword? && data != [] ->
        {:error, "Relationship change invalid for #{relationship.name} 1"}

      keyword? ->
        {:ok, %{replace: [data]}}

      is_list(data) ->
        validate_list_replace(relationship, data)

      !is_map(data) ->
        validate_map_replace(relationship, data)

      Map.get(data, :__struct__) ->
        validate_struct_replace(relationship, data, action_type)

      true ->
        validate_specified_replace(relationship, data, action_type)
    end
  end

  defp validate_specified_replace(relationship, data, action_type) do
    cond do
      Map.has_key?(data, :remove) and action_type == :create ->
        {:error, "Cannot remove from a relationship on create."}

      Map.has_key?(data, :replace) and action_type == :create ->
        data = Map.put(data, :add, data.replace)
        validate_relationship_change(relationship, data, action_type)

      relationship.cardinality == :one ->
        validate_to_one_relationship_data(relationship, data)

      relationship.cardinality == :many ->
        validate_to_many_relationship_data(relationship, data)
    end
  end

  defp validate_struct_replace(relationship, data, action_type) do
    new_data =
      data
      |> Map.take(Ash.primary_key(data.__struct__))
      |> Map.to_list()

    validate_relationship_change(relationship, new_data, action_type)
  end

  defp validate_map_replace(relationship, data) do
    case PrimaryKeyHelpers.value_to_primary_key_filter(
           relationship.destination,
           data
         ) do
      {:ok, identifier} ->
        {:ok, %{replace: identifier}}

      {:error, _} ->
        {:error, "Relationship change invalid for #{relationship.name} 2"}
    end
  end

  defp validate_list_replace(relationship, data) do
    case PrimaryKeyHelpers.values_to_primary_key_filters(
           relationship.destination,
           data
         ) do
      {:ok, identifiers} ->
        {:ok, %{replace: identifiers}}

      {:error, _} ->
        {:error, "Relationship change invalid for #{relationship.name} 2"}
    end
  end

  defp validate_to_one_relationship_data(%{destination: destination}, data) do
    keys = Enum.sort(Map.keys(data))

    case keys do
      [:add, :remove] ->
        with {:ok, add} <-
               PrimaryKeyHelpers.value_to_primary_key_filter(destination, data.add),
             {:remove, remove} <-
               PrimaryKeyHelpers.value_to_primary_key_filter(destination, data.remove) do
          {:ok, %{add: add, remove: remove}}
        end

      [key] when key in [:add, :replace] ->
        case PrimaryKeyHelpers.value_to_primary_key_filter(
               destination,
               Map.get(data, key)
             ) do
          {:ok, add} -> {:ok, %{replace: add}}
          {:error, error} -> {:error, error}
        end

      [:remove] ->
        case PrimaryKeyHelpers.value_to_primary_key_filter(destination, data.remove) do
          {:ok, remove} -> {:ok, %{remove: remove}}
          {:error, error} -> {:error, error}
        end
    end
  end

  defp validate_to_many_relationship_data(%{destination: destination}, data) do
    keys = Enum.sort(Map.keys(data))

    case keys do
      [:add, :remove] ->
        with {:ok, add} <-
               PrimaryKeyHelpers.values_to_primary_key_filters(destination, data.add),
             {:remove, remove} <-
               PrimaryKeyHelpers.values_to_primary_key_filters(
                 destination,
                 data.remove
               ) do
          {:ok, %{add: add, remove: remove}}
        end

      [key] when key in [:add, :replace, :remove] ->
        case PrimaryKeyHelpers.values_to_primary_key_filters(
               destination,
               Map.get(data, key)
             ) do
          {:ok, add} -> {:ok, %{key => add}}
          {:error, error} -> {:error, error}
        end
    end
  end

  def changeset(changeset, api, relationships) do
    if relationships == %{} do
      changeset
    else
      dependencies = Map.get(changeset, :__changes_depend_on__, [])

      Request.resolve(dependencies, fn data ->
        new_changeset =
          data
          |> Map.get(:relationships, %{})
          |> Enum.reduce(changeset, fn {relationship, relationship_data}, changeset ->
            relationship = Ash.relationship(changeset.data.__struct__, relationship)

            relationship_data =
              relationship_data
              |> Enum.into(%{}, fn {key, value} ->
                {key, value.data}
              end)
              |> Map.put_new(:current, [])

            add_relationship_to_changeset(changeset, api, relationship, relationship_data)
          end)

        {:ok, new_changeset}
      end)
    end
  end

  defp add_relationship_to_changeset(
         changeset,
         api,
         %{type: :has_one, destination: destination} = relationship,
         relationship_data
       ) do
    pkey = Ash.primary_key(destination)

    case relationship_data do
      %{current: [], replace: [new]} ->
        changeset
        |> relate_has_one(api, relationship, new)
        |> add_relationship_change_metadata(relationship.name, %{add: [new]})

      %{current: [current], replace: []} ->
        changeset
        |> unrelate_has_one(api, relationship, current)
        |> relate_has_one(api, relationship, nil)
        |> add_relationship_change_metadata(relationship.name, %{remove: [current]})

      %{current: [current], replace: [new]} ->
        changeset
        |> unrelate_has_one(api, relationship, current)
        |> relate_has_one(api, relationship, new)
        |> add_relationship_change_metadata(relationship.name, %{remove: [current], add: [new]})

      %{current: [current], add: [add]} ->
        if Map.take(current, pkey) == Map.take(add, pkey) do
          relate_has_one(changeset, api, relationship, current)
        else
          Ecto.Changeset.add_error(
            changeset,
            relationship.name,
            "Can't add a value to a belongs to when something is already related."
          )
        end

      %{current: [], remove: [_]} ->
        Ecto.Changeset.add_error(
          changeset,
          relationship.name,
          "Can't remove a value from a belongs to when nothing is related"
        )

      %{current: [current], remove: [remove]} ->
        if Map.take(current, pkey) == Map.take(remove, pkey) do
          changeset
          |> unrelate_has_one(api, relationship, current)
          |> relate_has_one(api, relationship, nil)
          |> add_relationship_change_metadata(relationship.name, %{remove: [current]})
        else
          Ecto.Changeset.add_error(
            changeset,
            relationship.name,
            "Can't remove a has_one related value if a different record is related"
          )
        end

      %{add: [new]} ->
        changeset
        |> relate_has_one(api, relationship, new)
        |> add_relationship_change_metadata(relationship.name, %{add: [new]})
    end
  end

  defp add_relationship_to_changeset(
         changeset,
         _api,
         %{type: :belongs_to, destination: destination} = relationship,
         relationship_data
       ) do
    pkey = Ash.primary_key(destination)

    relationship_data = Map.put_new(relationship_data, :current, [])

    case relationship_data do
      %{current: [], replace: [new]} ->
        changeset
        |> relate_belongs_to(relationship, new)
        |> add_relationship_change_metadata(relationship.name, %{add: [new]})

      %{current: [], add: [new]} ->
        changeset
        |> relate_belongs_to(relationship, new)
        |> add_relationship_change_metadata(relationship.name, %{add: [new]})

      %{current: [current], replace: []} ->
        changeset
        |> relate_belongs_to(relationship, nil)
        |> add_relationship_change_metadata(relationship.name, %{remove: [current]})

      %{current: [current], replace: [new]} ->
        changeset
        |> relate_belongs_to(relationship, new)
        |> add_relationship_change_metadata(relationship.name, %{remove: [current], add: [new]})

      %{current: [current], add: [add]} ->
        if Map.take(current, pkey) == Map.take(add, pkey) do
          relate_belongs_to(changeset, relationship, current)
        else
          Ecto.Changeset.add_error(
            changeset,
            relationship.name,
            "Can't add a value to a belongs to when something is already related."
          )
        end

      %{current: [], remove: [_]} ->
        Ecto.Changeset.add_error(
          changeset,
          relationship.name,
          "Can't remove a value from a belongs to when nothing is related"
        )

      %{current: [current], remove: [remove]} ->
        if Map.take(current, pkey) == Map.take(remove, pkey) do
          changeset
          |> add_relationship_change_metadata(relationship.name, %{remove: [current]})
          |> relate_belongs_to(relationship, nil)
        else
          Ecto.Changeset.add_error(
            changeset,
            relationship.name,
            "Can't remove a belongs_to related value if a different record is related"
          )
        end
    end
  end

  defp add_relationship_to_changeset(
         changeset,
         api,
         %{type: :has_many, destination: destination} = relationship,
         relationship_data
       ) do
    pkey = Ash.primary_key(destination)

    relationship_data =
      case relationship_data do
        %{replace: values, current: current} ->
          split_relationship_data(current, values, pkey)

        other ->
          other
      end

    changeset
    |> set_relationship(relationship.name, Map.get(relationship_data, :current, []))
    |> remove_has_many(api, relationship, relationship_data, pkey)
    |> relate_has_many(api, relationship, relationship_data, pkey)
  end

  defp add_relationship_to_changeset(
         changeset,
         api,
         %{type: :many_to_many, destination: destination} = relationship,
         relationship_data
       ) do
    pkey = Ash.primary_key(destination)

    relationship_data =
      case relationship_data do
        %{replace: values, current: current} ->
          split_relationship_data(current, values, pkey)

        other ->
          other
      end

    changeset
    |> set_relationship(relationship.name, Map.get(relationship_data, :current, []))
    |> remove_many_to_many(api, relationship, relationship_data, pkey)
    |> relate_many_to_many(api, relationship, relationship_data, pkey)
  end

  defp split_relationship_data(current, replace, pkey) do
    adding = Enum.reject(replace, &any_pkey_matches?(current, &1, pkey))

    removing = Enum.reject(current, &any_pkey_matches?(replace, &1, pkey))

    %{add: adding, remove: removing, current: current}
  end

  defp relate_many_to_many(changeset, api, relationship, %{add: add, current: current}, pkey)
       when is_list(add) do
    Enum.reduce(add, changeset, fn to_relate_record, changeset ->
      case find_pkey_match(current, to_relate_record, pkey) do
        nil ->
          do_relate_many_to_many(api, changeset, relationship, to_relate_record)

        _record ->
          changeset
      end
    end)
  end

  defp relate_many_to_many(changeset, _, _, _, _) do
    changeset
  end

  defp do_relate_many_to_many(api, changeset, relationship, to_relate_record) do
    add_after_changes(changeset, fn _changeset, record ->
      join_attrs = %{
        relationship.source_field_on_join_table() => Map.get(record, relationship.source_field),
        relationship.destination_field_on_join_table() =>
          Map.get(to_relate_record, relationship.destination_field)
      }

      relationship.through
      |> api.create(attributes: join_attrs, upsert?: true)
      |> case do
        {:ok, _join_row} ->
          {:ok,
           add_to_set_relationship(
             record,
             relationship.name,
             to_relate_record
           )}

        {:error, error} ->
          {:error, error}
      end
    end)
  end

  defp remove_many_to_many(
         changeset,
         api,
         relationship,
         %{current: current, remove: remove},
         pkey
       ) do
    Enum.reduce(remove, changeset, fn to_remove_record, changeset ->
      case find_pkey_match(current, to_remove_record, pkey) do
        nil ->
          changeset

        to_remove_record ->
          do_remove_many_to_many(api, changeset, relationship, to_remove_record, pkey)
      end
    end)
  end

  defp remove_many_to_many(changeset, _, _, _, _) do
    changeset
  end

  defp do_remove_many_to_many(api, changeset, relationship, to_remove_record, pkey) do
    add_after_changes(changeset, fn _changeset, record ->
      filter = [
        {relationship.source_field_on_join_table, Map.get(record, relationship.source_field)},
        {
          relationship.destination_field_on_join_table,
          Map.get(to_remove_record, relationship.destination_field)
        }
      ]

      case api.get(relationship.through, filter) do
        {:ok, nil} ->
          changeset

        {:error, error} ->
          {:error, error}

        {:ok, found} ->
          destroy_and_remove(api, found, record, relationship, pkey)
      end
    end)
  end

  defp destroy_and_remove(api, found, record, relationship, pkey) do
    case api.destroy(found) do
      :ok ->
        {:ok,
         remove_from_set_relationship(
           record,
           relationship.name,
           found,
           pkey
         )}

      {:error, error} ->
        {:error, error}
    end
  end

  defp relate_has_many(changeset, api, relationship, %{add: add, current: current}, pkey)
       when is_list(add) do
    Enum.reduce(add, changeset, fn to_relate_record, changeset ->
      if any_pkey_matches?(current, to_relate_record, pkey) do
        changeset
      else
        add_after_changes(changeset, fn _changeset, record ->
          to_relate_record
          |> api.update(
            attributes: %{
              relationship.destination_field => Map.get(record, relationship.source_field)
            }
          )
          |> case do
            {:ok, related} ->
              {:ok, add_to_set_relationship(record, relationship.name, related)}

            {:error, error} ->
              {:error, error}
          end
        end)
      end
    end)
  end

  defp relate_has_many(changeset, _, _, _, _) do
    changeset
  end

  defp remove_has_many(changeset, api, relationship, %{current: current, remove: remove}, pkey) do
    Enum.reduce(remove, changeset, fn to_relate_record, changeset ->
      if any_pkey_matches?(current, to_relate_record, pkey) do
        add_after_changes(changeset, fn _changeset, record ->
          to_relate_record
          |> api.update(
            attributes: %{
              relationship.destination_field => nil
            }
          )
          |> case do
            {:ok, related} ->
              {:ok, remove_from_set_relationship(record, relationship.name, related, pkey)}

            {:error, error} ->
              {:error, error}
          end
        end)
      else
        changeset
      end
    end)
  end

  defp remove_has_many(changeset, _, _, _, _) do
    changeset
  end

  defp find_pkey_match(records, to_relate_record, pkey) do
    search_pkey = Map.take(to_relate_record, pkey)

    Enum.find(records, fn record ->
      Map.take(record, pkey) == search_pkey
    end)
  end

  defp any_pkey_matches?(records, to_relate_record, pkey) do
    not is_nil(find_pkey_match(records, to_relate_record, pkey))
  end

  defp set_relationship(changeset, relationship_name, value) do
    Map.update!(changeset, :data, fn data ->
      case value do
        values when is_list(values) ->
          Map.put(data, relationship_name, Enum.map(values, &clear_relationships/1))

        value ->
          Map.put(data, relationship_name, value)
      end
    end)
  end

  defp add_to_set_relationship(record, relationship_name, to_relate) do
    Map.update!(record, relationship_name, fn
      %Ecto.Association.NotLoaded{} -> [clear_relationships(to_relate)]
      set_relationship -> [clear_relationships(to_relate) | set_relationship]
    end)
  end

  defp remove_from_set_relationship(record, relationship_name, to_remove, pkey) do
    Map.update!(record, relationship_name, fn
      %Ecto.Association.NotLoaded{} ->
        []

      set_relationship ->
        search_pkey = Map.take(to_remove, pkey)

        Enum.reject(set_relationship, fn set -> Map.take(set, pkey) == search_pkey end)
    end)
  end

  defp relate_belongs_to(changeset, relationship, new) do
    changeset =
      if new do
        Ecto.Changeset.cast(
          changeset,
          %{
            relationship.source_field => Map.get(new, relationship.destination_field)
          },
          [relationship.source_field]
        )
      else
        Ecto.Changeset.cast(changeset, %{relationship.source_field => nil}, [
          relationship.source_field
        ])
      end

    add_after_changes(changeset, fn _changeset, result ->
      if new do
        {:ok, Map.put(result, relationship.name, clear_relationships(new))}
      else
        {:ok, Map.put(result, relationship.name, nil)}
      end
    end)
  end

  defp relate_has_one(changeset, api, relationship, to_relate_record) do
    add_after_changes(changeset, fn _changeset, record ->
      if to_relate_record do
        to_relate_record
        |> api.update(
          attributes: %{
            relationship.destination_field => Map.get(record, relationship.source_field)
          }
        )
        |> case do
          {:ok, related} ->
            {:ok, Map.put(record, relationship.name, clear_relationships(related))}

          {:error, error} ->
            {:error, error}
        end
      else
        {:ok, Map.put(record, relationship.name, nil)}
      end
    end)
  end

  defp unrelate_has_one(changeset, api, relationship, to_relate_record) do
    add_after_changes(changeset, fn _changeset, record ->
      to_relate_record
      |> api.update(
        attributes: %{
          relationship.destination_field => nil
        }
      )
      |> case do
        {:ok, _related} ->
          {:ok, record}

        {:error, error} ->
          {:error, error}
      end
    end)
  end

  defp add_after_changes(changeset, func) do
    Map.update(changeset, :__after_changes__, [func], fn funcs -> [func | funcs] end)
  end

  defp add_relationship_change_metadata(changeset, relationship_name, data) do
    Map.update(
      changeset,
      :__ash_relationships__,
      %{relationship_name => data},
      &Map.put(&1, relationship_name, data)
    )
  end

  defp fetch_string_or_atom(map, name) do
    case Map.fetch(map, name) do
      {:ok, value} -> {:ok, value}
      :error -> Map.fetch(map, to_string(name))
    end
  end

  defp add_relationship_currently_related_request(
         changeset,
         api,
         %{type: :many_to_many} = relationship
       ) do
    join_through_request = many_to_many_join_resource_request(api, changeset, relationship)

    destination_request = many_to_many_destination_request(api, relationship)

    requests = [join_through_request, destination_request]

    changeset
    |> add_requests(requests)
    |> changes_depend_on([:relationships, relationship.name, :current, :data])
  end

  defp add_relationship_currently_related_request(
         changeset,
         api,
         %{destination: destination} = relationship
       ) do
    value = Ecto.Changeset.get_field(changeset, relationship.source_field)
    filter_statement = [{relationship.destination_field, value}]

    query =
      api
      |> Ash.Query.new(destination)
      |> Ash.Query.filter(filter_statement)

    request =
      Request.new(
        api: api,
        resource: destination,
        action: Ash.primary_action!(relationship.destination, :read),
        path: [:relationships, relationship.name, :current],
        query: query,
        data:
          Request.resolve(fn _data ->
            api.read(query)
          end),
        name: "Read related #{relationship.name} before replace"
      )

    changeset
    |> add_requests(request)
    |> changes_depend_on([:relationships, relationship.name, :current, :data])
  end

  defp many_to_many_join_resource_request(
         api,
         changeset,
         %{through: through} = relationship
       ) do
    value = Ecto.Changeset.get_field(changeset, relationship.source_field)
    filter_statement = [{relationship.source_field_on_join_table, value}]

    query =
      api
      |> Ash.Query.new(through)
      |> Ash.Query.filter(filter_statement)

    Request.new(
      api: api,
      resource: through,
      action: Ash.primary_action!(relationship.destination, :read),
      path: [:relationships, relationship.name, :current_join],
      query: query,
      data:
        Request.resolve(fn _data ->
          api.read(query)
        end),
      name: "Read related join for #{relationship.name} before replace"
    )
  end

  defp many_to_many_destination_request(
         api,
         %{destination: destination, name: name} = relationship
       ) do
    Request.new(
      api: api,
      resource: destination,
      action: Ash.primary_action!(relationship.destination, :read),
      path: [:relationships, name, :current],
      query:
        Request.resolve(
          [[:relationships, name, :current_join, :data]],
          fn %{relationships: %{^name => %{current_join: %{data: current_join}}}} ->
            field_values =
              Enum.map(current_join, &Map.get(&1, relationship.destination_field_on_join_table))

            filter_statement = [{relationship.destination_field, in: field_values}]

            {:ok,
             relationship.destination
             |> api.query()
             |> Ash.Query.filter(filter_statement)}
          end
        ),
      data:
        Request.resolve(
          [[:relationships, name, :current, :query]],
          fn %{
               relationships: %{
                 ^name => %{current: %{query: query}}
               }
             } ->
            api.read(query)
          end
        ),
      name: "Read related join for #{name} before replace"
    )
  end

  defp changes_depend_on(changeset, path) do
    Map.update(changeset, :__changes_depend_on__, [path], fn paths -> [path | paths] end)
  end

  defp add_requests(changeset, requests) do
    requests = List.wrap(requests)
    Map.update(changeset, :__requests__, requests, &Kernel.++(&1, requests))
  end

  defp clear_relationships(%resource{} = record) do
    resource
    |> Ash.relationships()
    |> Enum.reduce(record, fn relationship, record ->
      not_loaded = %Ecto.Association.NotLoaded{
        __cardinality__: relationship.cardinality,
        __field__: relationship.name,
        __owner__: relationship.source
      }

      Map.put(record, relationship.name, not_loaded)
    end)
  end
end
