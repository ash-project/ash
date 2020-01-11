defmodule Ash.Actions.Relationships do
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
          case validate_relationship_change(relationship, data, action_type) do
            {:ok, input} ->
              add_relationship_read_authorizations(changeset, api, relationship, input)

            {:error, error} ->
              {:error, error}
          end
      end
    end)
  end

  def relationship_change_authorizations(changeset, api, resource, action, relationships) do
    Enum.flat_map(relationships, fn {relationship_name, _data} ->
      case Ash.relationship(resource, relationship_name) do
        nil ->
          []

        relationship ->
          authorization =
            Ash.Authorization.Request.new(
              api: api,
              rules: relationship.write_rules,
              resource: resource,
              changeset: authorization_changeset(changeset, relationships),
              action_type: action.type,
              fetcher: fn _, %{data: data} ->
                {:ok, data}
              end,
              dependencies: [:data | Map.get(changeset, :__changes_depend_on__, [])],
              state_key: :data,
              must_fetch?: false,
              relationship: [],
              source: "#{relationship_name} edit"
            )

          [authorization]
      end
    end)
  end

  defp add_relationship_read_authorizations(changeset, api, relationship, input) do
    changeset
    |> add_replace_authorizations(api, relationship, input)
    |> add_remove_authorizations(api, relationship, input)
    |> add_add_authorizations(api, relationship, input)
  end

  defp add_add_authorizations(changeset, api, relationship, input) do
    case Map.fetch(input, :add) do
      {:ok, identifiers} ->
        changeset =
          case relationship do
            %{type: :belongs_to, source_field: source_field, destination_field: destination_field} ->
              case Keyword.fetch(identifiers, destination_field) do
                {:ok, field_value} ->
                  changeset
                  |> Ecto.Changeset.put_change(source_field, field_value)
                  |> Map.put_new(:__ash_skip_authorization_fields__, [])
                  |> Map.update!(:__ash_skip_authorization_fields__, fn fields ->
                    [source_field | fields]
                  end)

                _ ->
                  changeset
              end

            _ ->
              changeset
          end

        do_add_relationship_read_authorizations(changeset, api, relationship, identifiers, :add)

      :error ->
        changeset
    end
  end

  defp add_replace_authorizations(changeset, api, relationship, input) do
    case Map.fetch(input, :replace) do
      {:ok, identifiers} ->
        changeset
        |> do_add_relationship_read_authorizations(api, relationship, identifiers, :replace)
        |> add_relationship_currently_related_authorization(api, relationship)

      :error ->
        changeset
    end
  end

  defp add_remove_authorizations(changeset, api, relationship, input) do
    case Map.fetch(input, :remove) do
      {:ok, identifiers} ->
        changeset =
          case relationship do
            %{type: :belongs_to, source_field: source_field} ->
              changeset
              |> Ecto.Changeset.put_change(source_field, nil)
              |> Map.put_new(:__ash_skip_authorization_fields__, [])
              |> Map.update!(:__ash_skip_authorization_fields__, fn fields ->
                [source_field | fields]
              end)

            _ ->
              changeset
          end

        do_add_relationship_read_authorizations(
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

  defp do_add_relationship_read_authorizations(
         changeset,
         api,
         %{destination: destination} = relationship,
         identifiers,
         type
       ) do
    default_read =
      Ash.primary_action(destination, :read) ||
        raise "Need a default read action for #{destination}"

    relationship_name = relationship.name

    filter =
      case relationship.cardinality do
        :many ->
          case identifiers do
            [single_identifier] ->
              single_identifier

            many ->
              [or: many]
          end

        :one ->
          identifiers
      end

    authorization =
      Ash.Authorization.Request.new(
        api: api,
        rules: default_read.rules,
        resource: relationship.destination,
        action_type: :read,
        filter: filter,
        must_fetch?: true,
        state_key: [:relationships, relationship_name, type],
        fetcher: fn _, _ ->
          case api.read(destination, filter: filter, paginate: false) do
            {:ok, %{results: results}} -> {:ok, results}
            {:error, error} -> {:error, error}
          end
        end,
        relationship: [relationship.name],
        source: "read prior to write related #{relationship.name}"
      )

    changeset
    |> add_authorizations(authorization)
    |> changes_depend_on([:relationships, relationship_name, type])
  end

  defp validate_relationship_change(relationship, data, action_type) do
    keyword? = Keyword.keyword?(data)

    cond do
      relationship.cardinality == :many && keyword? ->
        {:error, "Relationship change invalid for #{relationship.name} 1"}

      keyword? ->
        {:ok, %{replace: [data]}}

      is_list(data) ->
        case Ash.Actions.PrimaryKeyHelpers.values_to_primary_key_filters(
               relationship.destination,
               data
             ) do
          {:ok, identifier} ->
            {:ok, %{replace: identifier}}

          {:error, _} ->
            {:error, "Relationship change invalid for #{relationship.name} 2"}
        end

      !is_map(data) ->
        case Ash.Actions.PrimaryKeyHelpers.value_to_primary_key_filter(
               relationship.destination,
               data
             ) do
          {:ok, identifier} ->
            {:ok, %{replace: identifier}}

          {:error, _} ->
            {:error, "Relationship change invalid for #{relationship.name} 2"}
        end

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

  defp validate_to_one_relationship_data(%{destination: destination}, data) do
    keys = Enum.sort(Map.keys(data))

    case keys do
      [:add, :remove] ->
        with {:ok, add} <-
               Ash.Actions.PrimaryKeyHelpers.value_to_primary_key_filter(destination, data.add),
             {:remove, remove} <-
               Ash.Actions.PrimaryKeyHelpers.value_to_primary_key_filter(destination, data.remove) do
          {:ok, %{add: add, remove: remove}}
        end

      [key] when key in [:add, :replace] ->
        case Ash.Actions.PrimaryKeyHelpers.value_to_primary_key_filter(
               destination,
               Map.get(data, key)
             ) do
          {:ok, add} -> {:ok, %{replace: add}}
          {:error, error} -> {:error, error}
        end

      [:remove] ->
        case Ash.Actions.PrimaryKeyHelpers.value_to_primary_key_filter(destination, data.remove) do
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
               Ash.Actions.PrimaryKeyHelpers.values_to_primary_key_filters(destination, data.add),
             {:remove, remove} <-
               Ash.Actions.PrimaryKeyHelpers.values_to_primary_key_filters(
                 destination,
                 data.remove
               ) do
          {:ok, %{add: add, remove: remove}}
        end

      [key] when key in [:add, :replace, :remove] ->
        case Ash.Actions.PrimaryKeyHelpers.values_to_primary_key_filters(
               destination,
               Map.get(data, key)
             ) do
          {:ok, add} -> {:ok, %{key => add}}
          {:error, error} -> {:error, error}
        end
    end
  end

  def authorization_changeset(changeset, relationships) do
    if relationships == %{} do
      changeset
    else
      fn data ->
        data
        |> Map.get(:relationships, %{})
        |> Enum.reduce(changeset, fn {relationship, relationship_data}, changeset ->
          relationship = Ash.relationship(changeset.data.__struct__, relationship)

          add_relationship_to_changeset(changeset, relationship, relationship_data)
        end)
      end
    end
  end

  defp add_relationship_to_changeset(
         changeset,
         %{type: :belongs_to, destination: destination} = relationship,
         relationship_data
       ) do
    pkey = Ash.primary_key(destination)

    case relationship_data do
      %{current: [], replace: [new]} ->
        changeset
        |> Ecto.Changeset.put_change(
          relationship.source_field,
          Map.get(new, relationship.destination_field)
        )
        |> add_relationship_change_metadata(relationship.name, %{add: [new]})

      %{current: [current], replace: []} ->
        changeset
        |> Ecto.Changeset.put_change(
          relationship.source_field,
          nil
        )
        |> add_relationship_change_metadata(relationship.name, %{remove: [current]})

      %{current: [current], replace: [new]} ->
        changeset
        |> Ecto.Changeset.put_change(
          relationship.source_field,
          Map.get(new, relationship.destination_field)
        )
        |> add_relationship_change_metadata(relationship.name, %{remove: [current], add: [new]})

      %{current: [current], add: [add]} ->
        if Map.take(current, pkey) == Map.take(add, pkey) do
          changeset
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
          Ecto.Changeset.put_change(changeset, relationship.source_field, nil)
        else
          Ecto.Changeset.add_error(
            changeset,
            relationship.name,
            "Can't remove a related value if a different record is related"
          )
        end
    end
  end

  defp add_relationship_to_changeset(changeset, relationship, relationship_data) do
    IO.inspect(relationship, label: "relationship")
    IO.inspect(relationship_data, label: "relationship data")

    {:ok, changeset}
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

  defp add_relationship_currently_related_authorization(
         changeset,
         api,
         %{destination: destination} = relationship
       ) do
    default_read =
      Ash.primary_action(destination, :read) ||
        raise "Must have a default read for #{destination}"

    value = Ecto.Changeset.get_field(changeset, relationship.source_field)
    filter_statement = [{relationship.destination_field, value}]
    filter = Ash.Filter.parse(destination, filter_statement)

    authorization =
      Ash.Authorization.Request.new(
        api: api,
        rules: default_read.rules,
        resource: destination,
        action_type: :read,
        state_key: [:relationships, relationship.name, :current],
        must_fetch?: true,
        filter: filter,
        fetcher: fn _, _ ->
          case api.read(destination, filter: filter_statement) do
            {:ok, %{results: results}} -> {:ok, results}
            {:error, error} -> {:error, error}
          end
        end,
        relationship: [],
        bypass_strict_access?: true,
        source: "Read related #{relationship.name} before replace"
      )

    changeset
    |> add_authorizations(authorization)
    |> changes_depend_on([:relationships, relationship.name, :current])
  end

  defp changes_depend_on(changeset, path) do
    Map.update(changeset, :__changes_depend_on__, [path], fn paths -> [path | paths] end)
  end

  defp add_authorizations(changeset, authorizations) do
    authorizations = List.wrap(authorizations)
    Map.update(changeset, :__authorizations__, authorizations, &Kernel.++(&1, authorizations))
  end
end
