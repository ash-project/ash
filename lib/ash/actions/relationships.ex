defmodule Ash.Actions.Relationships do
  @moduledoc false
  alias Ash.Changeset
  alias Ash.Engine.Request
  require Ash.Query

  def handle_relationship_changes(changeset) do
    Enum.reduce(changeset.relationships, changeset, fn {name, data}, changeset ->
      relationship = Ash.Resource.relationship(changeset.resource, name)
      add_relationship_read_requests(changeset, relationship, data)
    end)
  end

  defp add_relationship_read_requests(%{action_type: :update} = changeset, relationship, input) do
    changeset
    |> add_replace_requests(relationship, input)
    |> add_remove_requests(relationship, input)
    |> add_add_requests(relationship, input)
    |> add_belongs_to_change(relationship, input)
  end

  defp add_relationship_read_requests(%{action_type: :create} = changeset, relationship, input) do
    {input, key} =
      if relationship.cardinality == :many do
        case Map.fetch(input, :replace) do
          {:ok, replacing} ->
            {Map.update(input, :add, replacing, &Kernel.++(&1, replacing)), :add}

          _ ->
            {input, :add}
        end
      else
        {input, :replace}
      end

    changeset
    |> add_add_requests(relationship, Map.delete(input, :remove), key)
    |> add_belongs_to_change(relationship, input)
  end

  defp add_belongs_to_change(
         changeset,
         %{type: :belongs_to, source_field: source_field, destination_field: destination_field},
         input
       ) do
    case Map.fetch(input, :replace) do
      {:ok, nil} ->
        changeset

      :error ->
        changeset

      {:ok, replace} ->
        add_belongs_to_change(changeset, replace, source_field, destination_field)
    end
  end

  defp add_belongs_to_change(changeset, _, _), do: changeset

  defp add_add_requests(changeset, relationship, input, key \\ :add) do
    case Map.fetch(input, key) do
      {:ok, identifiers} ->
        do_add_relationship_read_requests(changeset, relationship, identifiers, key)

      :error ->
        changeset
    end
  end

  defp add_belongs_to_change(changeset, identifiers, source_field, destination_field) do
    case Map.get(identifiers, destination_field) do
      {:ok, field_value} ->
        Changeset.force_change_attribute(changeset, source_field, field_value)

      _ ->
        changeset
    end
  end

  defp add_replace_requests(changeset, relationship, input) do
    case Map.fetch(input, :replace) do
      {:ok, identifiers} ->
        changeset
        |> do_add_relationship_read_requests(relationship, identifiers, :replace)
        |> add_relationship_currently_related_request(relationship)

      :error ->
        changeset
    end
  end

  defp add_remove_requests(changeset, relationship, input) do
    case Map.fetch(input, :remove) do
      {:ok, identifiers} ->
        do_add_relationship_read_requests(
          changeset,
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
         %{destination: destination} = relationship,
         identifiers,
         type
       ) do
    relationship_name = relationship.name

    {possible?, filter} =
      case identifiers do
        [{single_identifier, _changeset}] ->
          {true, single_identifier}

        [single_identifier] ->
          {true, single_identifier}

        [] ->
          {false, []}

        single when is_map(single) ->
          {true, Map.to_list(single)}

        many ->
          case Ash.Resource.primary_key(relationship.destination) do
            [field] ->
              {true, [{field, in: get_many_field(many, field)}]}

            _ ->
              {true, [or: get_many_records(many)]}
          end
      end

    query = Ash.Query.filter(destination, ^filter)

    dependencies =
      if possible? do
        [[:relationships, relationship_name, type, :query]]
      else
        []
      end

    request =
      Request.new(
        api: changeset.api,
        resource: relationship.destination,
        action: Ash.Resource.primary_action!(relationship.destination, :read),
        query: query,
        path: [:relationships, relationship_name, type],
        async?: not possible?,
        authorize?: possible?,
        data:
          get_in(changeset.context, [:destination_entities, relationship.name, relationship.destination]) ||
            Request.resolve(dependencies, fn data ->
              if possible? do
                query = get_in(data, [:relationships, relationship_name, type, :query])

                case Ash.Actions.Read.unpaginated_read(query) do
                  {:ok, results} ->
                    {:ok, add_changes_to_results(changeset.resource, results, identifiers)}

                  {:error, error} ->
                    {:error, error}
                end
              else
                {:ok, []}
              end
            end),
        name: "read prior to write related #{relationship.name}"
      )

    changeset
    |> Changeset.add_requests(request)
    |> Changeset.changes_depend_on([:relationships, relationship_name, type, :data])
  end

  defp add_changes_to_results(resource, results, identifiers) do
    pkey = Ash.Resource.primary_key(resource)

    Enum.map(results, fn result ->
      case find_changes(identifiers, result, pkey) do
        nil ->
          result

        changes ->
          {result, changes}
      end
    end)
  end

  defp find_changes(identifiers, result, pkey) do
    Enum.find_value(identifiers, fn
      {identifier, changes} ->
        cond do
          is_map(identifier) && Map.take(identifier, pkey) == Map.take(result, pkey) ->
            changes

          match?([_], pkey) && identifier == Map.get(result, List.first(pkey)) ->
            changes

          true ->
            nil
        end

      _ ->
        nil
    end)
  end

  defp get_many_field(records, field) do
    Enum.map(records, fn
      {record, _changes} ->
        Map.get(record, field)

      record ->
        Map.get(record, field)
    end)
  end

  defp get_many_records(records) do
    Enum.map(records, fn
      {record, _changes} ->
        record

      record ->
        record
    end)
  end

  def changeset(changeset) do
    if changeset.relationships == %{} do
      changeset
    else
      Request.resolve(changeset.change_dependencies, fn data ->
        new_changeset =
          data
          |> Map.get(:relationships, %{})
          |> Enum.reduce(changeset, fn {relationship, relationship_data}, changeset ->
            relationship = Ash.Resource.relationship(changeset.resource, relationship)

            relationship_data =
              relationship_data
              |> Enum.into(%{}, fn {key, value} ->
                {key, value.data}
              end)
              |> Map.put_new(:current, [])

            add_relationship_to_changeset(changeset, relationship, relationship_data)
          end)

        {:ok, new_changeset}
      end)
    end
  end

  defp add_relationship_to_changeset(
         changeset,
         %{type: :has_one} = relationship,
         relationship_data
       ) do
    case relationship_data do
      %{current: [current], replace: []} ->
        changeset
        |> unrelate_has_one(relationship, current)
        |> relate_has_one(relationship, nil)
        |> add_relationship_change_metadata(relationship.name, %{current: current, replace: nil})

      %{current: [current], replace: [new]} ->
        changeset
        |> unrelate_has_one(relationship, current)
        |> relate_has_one(relationship, new)
        |> add_relationship_change_metadata(relationship.name, %{current: current, replace: new})

      %{current: [], replace: [new]} ->
        changeset
        |> relate_has_one(relationship, new)
        |> add_relationship_change_metadata(relationship.name, %{current: nil, replace: new})
    end
  end

  defp add_relationship_to_changeset(
         changeset,
         %{type: :belongs_to} = relationship,
         relationship_data
       ) do
    relationship_data = Map.put_new(relationship_data, :current, [])

    case relationship_data do
      %{current: [current], replace: []} ->
        changeset
        |> relate_belongs_to(relationship, nil)
        |> add_relationship_change_metadata(relationship.name, %{current: current, replace: nil})

      %{current: [], replace: [new]} ->
        changeset
        |> relate_belongs_to(relationship, new)
        |> add_relationship_change_metadata(relationship.name, %{current: nil, replace: new})

      %{current: [current], replace: [new]} ->
        changeset
        |> relate_belongs_to(relationship, new)
        |> add_relationship_change_metadata(relationship.name, %{
          current: current,
          replace: new
        })
    end
  end

  defp add_relationship_to_changeset(
         changeset,
         %{type: :has_many, destination: destination} = relationship,
         relationship_data
       ) do
    pkey = Ash.Resource.primary_key(destination)

    relationship_data =
      case relationship_data do
        %{replace: values, current: current} ->
          split_relationship_data(current, values, pkey)

        other ->
          other
      end

    changeset
    |> set_relationship(relationship.name, Map.get(relationship_data, :current, []))
    |> remove_has_many(relationship, relationship_data, pkey)
    |> relate_has_many(relationship, relationship_data, pkey)
    |> add_relationship_change_metadata(relationship.name, relationship_data)
  end

  defp add_relationship_to_changeset(
         changeset,
         %{type: :many_to_many, destination: destination} = relationship,
         relationship_data
       ) do
    pkey = Ash.Resource.primary_key(destination)
    join_pkey = Ash.Resource.primary_key(relationship.through)

    relationship_data =
      case relationship_data do
        %{replace: values, current: current} ->
          split_relationship_data(current, values, pkey)

        other ->
          other
      end

    changeset
    |> set_relationship(relationship.name, Map.get(relationship_data, :current, []))
    |> remove_many_to_many(relationship, relationship_data, join_pkey, pkey)
    |> relate_many_to_many(relationship, relationship_data, pkey)
    |> add_relationship_change_metadata(relationship.name, relationship_data)
  end

  defp split_relationship_data(current, replace, pkey) do
    adding =
      Enum.reject(replace, fn
        {_, _} ->
          false

        replacing ->
          any_pkey_matches?(current, replacing, pkey)
      end)

    removing = Enum.reject(current, &any_pkey_matches?(replace, &1, pkey))

    %{add: adding, remove: removing, current: current}
  end

  defp relate_many_to_many(
         changeset,
         relationship,
         %{add: add, current: current},
         pkey
       )
       when is_list(add) do
    Enum.reduce(add, changeset, fn
      {to_relate_record, join_changeset}, changeset ->
        do_relate_many_to_many(changeset, relationship, to_relate_record, join_changeset)

      to_relate_record, changeset ->
        case find_pkey_match(current, to_relate_record, pkey) do
          nil ->
            do_relate_many_to_many(changeset, relationship, to_relate_record)

          _record ->
            changeset
        end
    end)
  end

  defp relate_many_to_many(changeset, _, _, _) do
    changeset
  end

  defp do_relate_many_to_many(changeset, relationship, to_relate_record, join_changeset \\ nil) do
    Changeset.after_action(changeset, fn changeset, record ->
      join_attrs = %{
        relationship.source_field_on_join_table => Map.get(record, relationship.source_field),
        relationship.destination_field_on_join_table =>
          Map.get(to_relate_record, relationship.destination_field)
      }

      join_changeset
      |> Kernel.||(Ash.Changeset.new(relationship.through))
      |> Ash.Changeset.force_change_attributes(join_attrs)
      |> changeset.api.create(upsert?: true, return_notifications?: true)
      |> case do
        {:ok, join_row, notifications} ->
          {:ok,
           record
           |> remove_from_set_relationship(
             relationship.name,
             to_relate_record,
             Ash.Resource.primary_key(relationship.destination)
           )
           |> add_to_set_relationship(relationship.name, to_relate_record)
           |> remove_from_set_relationship(
             relationship.join_relationship,
             join_row,
             Ash.Resource.primary_key(relationship.through)
           )
           |> add_to_set_relationship(relationship.join_relationship, join_row), notifications}

        {:error, error} ->
          {:error, error}
      end
    end)
  end

  defp remove_many_to_many(
         changeset,
         relationship,
         %{current: current, remove: remove},
         join_pkey,
         pkey
       ) do
    Enum.reduce(remove, changeset, fn to_remove_record, changeset ->
      case find_pkey_match(current, to_remove_record, pkey) do
        nil ->
          changeset

        to_remove_record ->
          do_remove_many_to_many(changeset, relationship, to_remove_record, join_pkey, pkey)
      end
    end)
  end

  defp remove_many_to_many(changeset, _, _, _, _) do
    changeset
  end

  defp do_remove_many_to_many(changeset, relationship, to_remove_record, join_pkey, pkey) do
    Changeset.after_action(changeset, fn changeset, record ->
      filter = [
        {relationship.source_field_on_join_table, Map.get(record, relationship.source_field)},
        {
          relationship.destination_field_on_join_table,
          Map.get(to_remove_record, relationship.destination_field)
        }
      ]

      case changeset.api.get(relationship.through, filter) do
        {:ok, nil} ->
          changeset

        {:error, error} ->
          {:error, error}

        {:ok, join_row} ->
          destroy_and_remove(
            changeset.api,
            join_row,
            to_remove_record,
            record,
            relationship,
            join_pkey,
            pkey
          )
      end
    end)
  end

  defp destroy_and_remove(api, join_row, to_remove_record, record, relationship, join_pkey, pkey) do
    case api.destroy(Ash.Changeset.new(join_row), return_notifications?: true) do
      {:ok, notifications} ->
        {:ok,
         record
         |> remove_from_set_relationship(relationship.join_relationship, join_row, join_pkey)
         |> remove_from_set_relationship(relationship.name, to_remove_record, pkey),
         notifications}

      {:error, error} ->
        {:error, error}
    end
  end

  defp relate_has_many(changeset, relationship, %{add: add, current: current}, pkey)
       when is_list(add) do
    Enum.reduce(add, changeset, fn to_relate_record, changeset ->
      if any_pkey_matches?(current, to_relate_record, pkey) do
        changeset
      else
        Changeset.after_action(changeset, fn changeset, record ->
          to_relate_record
          |> Ash.Changeset.new()
          |> Ash.Changeset.force_change_attribute(
            relationship.destination_field,
            Map.get(record, relationship.source_field)
          )
          |> changeset.api.update(return_notifications?: true)
          |> case do
            {:ok, related, notifications} ->
              {:ok, add_to_set_relationship(record, relationship.name, related), notifications}

            {:error, error} ->
              {:error, error}
          end
        end)
      end
    end)
  end

  defp relate_has_many(changeset, _, _, _) do
    changeset
  end

  defp remove_has_many(changeset, relationship, %{current: current, remove: remove}, pkey) do
    Enum.reduce(remove, changeset, fn to_relate_record, changeset ->
      if any_pkey_matches?(current, to_relate_record, pkey) do
        Changeset.after_action(changeset, fn changeset, record ->
          to_relate_record
          |> Ash.Changeset.new()
          |> Ash.Changeset.force_change_attribute(relationship.destination_field, nil)
          |> changeset.api.update(return_notifications?: true)
          |> case do
            {:ok, related, notifications} ->
              {:ok, remove_from_set_relationship(record, relationship.name, related, pkey),
               notifications}

            {:error, error} ->
              {:error, error}
          end
        end)
      else
        changeset
      end
    end)
  end

  defp remove_has_many(changeset, _, _, _) do
    changeset
  end

  defp find_pkey_match(records, to_relate_record, pkey) do
    search_pkey = Map.take(to_relate_record, pkey)

    Enum.find(records, fn
      {record, _changes} ->
        Map.take(record, pkey) == search_pkey

      record ->
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
          Map.put(data, relationship_name, clear_relationships(value))
      end
    end)
  end

  defp add_to_set_relationship(record, relationship_name, to_relate) do
    Map.update!(record, relationship_name, fn
      %Ash.NotLoaded{type: :relationship} -> [clear_relationships(to_relate)]
      set_relationship -> [clear_relationships(to_relate) | set_relationship]
    end)
  end

  defp remove_from_set_relationship(record, relationship_name, to_remove, pkey) do
    Map.update!(record, relationship_name, fn
      %Ash.NotLoaded{} ->
        []

      set_relationship ->
        search_pkey = Map.take(to_remove, pkey)

        Enum.reject(set_relationship, fn set -> Map.take(set, pkey) == search_pkey end)
    end)
  end

  defp relate_belongs_to(changeset, relationship, new) do
    changeset =
      if new do
        Changeset.force_change_attribute(
          changeset,
          relationship.source_field,
          Map.get(new, relationship.destination_field)
        )
      else
        Changeset.force_change_attribute(changeset, relationship.source_field, nil)
      end

    Changeset.after_action(changeset, fn _changeset, result ->
      if new do
        {:ok, Map.put(result, relationship.name, clear_relationships(new))}
      else
        {:ok, Map.put(result, relationship.name, nil)}
      end
    end)
  end

  defp relate_has_one(changeset, relationship, to_relate_record) do
    Changeset.after_action(changeset, fn _changeset, record ->
      if to_relate_record do
        to_relate_record
        |> Ash.Changeset.new()
        |> Ash.Changeset.force_change_attribute(
          relationship.destination_field,
          Map.get(record, relationship.source_field)
        )
        |> changeset.api.update(return_notifications?: true)
        |> case do
          {:ok, related, notifications} ->
            {:ok, Map.put(record, relationship.name, clear_relationships(related)), notifications}

          {:error, error} ->
            {:error, error}
        end
      else
        {:ok, Map.put(record, relationship.name, nil)}
      end
    end)
  end

  defp unrelate_has_one(changeset, relationship, to_relate_record) do
    Changeset.after_action(changeset, fn changeset, record ->
      to_relate_record
      |> Changeset.new()
      |> Changeset.force_change_attribute(relationship.destination_field, nil)
      |> changeset.api.update(return_notifications?: true)
      |> case do
        {:ok, _related, notifications} ->
          {:ok, record, notifications}

        {:error, error} ->
          {:error, error}
      end
    end)
  end

  defp add_relationship_change_metadata(changeset, relationship_name, data) do
    Map.update(
      changeset,
      :relationships,
      %{relationship_name => data},
      &Map.put(&1, relationship_name, data)
    )
  end

  defp add_relationship_currently_related_request(
         changeset,
         %{type: :many_to_many} = relationship
       ) do
    join_through_request = many_to_many_join_resource_request(changeset, relationship)

    destination_request = many_to_many_destination_request(changeset.api, relationship)

    requests = [join_through_request, destination_request]

    changeset
    |> Changeset.add_requests(requests)
    |> Changeset.changes_depend_on([:relationships, relationship.name, :current, :data])
  end

  defp add_relationship_currently_related_request(
         changeset,
         %{destination: destination} = relationship
       ) do
    value = Changeset.get_attribute(changeset, relationship.source_field)
    filter_statement = [{relationship.destination_field, value}]

    request =
      Request.new(
        api: changeset.api,
        resource: destination,
        action: Ash.Resource.primary_action!(relationship.destination, :read),
        path: [:relationships, relationship.name, :current],
        query: Ash.Query.filter(destination, ^filter_statement),
        data:
          Request.resolve([[:relationships, relationship.name, :current, :query]], fn data ->
            query = get_in(data, [:relationships, relationship.name, :current, :query])

            Ash.Actions.Read.unpaginated_read(query)
          end),
        name: "Read related #{relationship.name} before replace"
      )

    changeset
    |> Changeset.add_requests(request)
    |> Changeset.changes_depend_on([:relationships, relationship.name, :current, :data])
  end

  defp many_to_many_join_resource_request(
         changeset,
         %{through: through} = relationship
       ) do
    value = Changeset.get_attribute(changeset, relationship.source_field)
    filter_statement = [{relationship.source_field_on_join_table, value}]

    Request.new(
      api: changeset.api,
      resource: through,
      action: Ash.Resource.primary_action!(relationship.destination, :read),
      path: [:relationships, relationship.name, :current_join],
      query: Ash.Query.filter(through, ^filter_statement),
      data:
        Request.resolve([[:relationships, relationship.name, :current_join, :query]], fn data ->
          query = get_in(data, [:relationships, relationship.name, :current_join, :query])
          Ash.Actions.Read.unpaginated_read(query)
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
      action: Ash.Resource.primary_action!(relationship.destination, :read),
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
             |> Ash.Query.new(api)
             |> Ash.Query.filter(^filter_statement)}
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
            Ash.Actions.Read.unpaginated_read(query)
          end
        ),
      name: "Read related join for #{name} before replace"
    )
  end

  defp clear_relationships(%resource{} = record) do
    resource
    |> Ash.Resource.relationships()
    |> Enum.reduce(record, fn relationship, record ->
      not_loaded = %Ash.NotLoaded{
        type: :relationship,
        field: relationship.name
      }

      Map.put(record, relationship.name, not_loaded)
    end)
  end
end
