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
        authorization_steps: default_read.authorization_steps,
        resource: relationship.destination,
        action_type: :read,
        filter: filter,
        must_fetch?: true,
        state_key: [:relationships, relationship_name, type],
        fetcher: fn _ ->
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
    # relationships_needing_field_update =
    #   relationships
    #   |> Enum.reduce_while({:ok, []}, fn {name, data}, {:ok, relationships} ->
    #     case Ash.relationship(changeset.data.__struct__, name) do
    #       nil ->
    #         {:halt, {:error, name}}

    #       %{type: :belongs_to, destination_field: destination_field} = relationship ->
    #         case identifier_or_identifiers(relationship, data) do
    #           {:ok, identifier} ->
    #             if Keyword.has_key?(identifier, destination_field) do
    #               {:cont, {:ok, relationships}}
    #             else
    #               {:cont, {:ok, [relationship | relationships]}}
    #             end

    #           {:error, _} ->
    #             {:halt, {:error, name}}
    #         end

    #       _ ->
    #         {:cont, {:ok, relationships}}
    #     end
    #   end)
    if relationships == %{} do
      changeset
    else
      fn data ->
        Enum.reduce_while(data.relationships, {:ok, changeset}, fn {relationship,
                                                                    relationship_data},
                                                                   {:ok, changeset} ->
          relationship = Ash.relationship(changeset.data.__struct__, relationship)

          case add_relationship_to_changeset(changeset, relationship, relationship_data) do
            {:ok, changeset} -> {:cont, {:ok, changeset}}
            {:error, error} -> {:halt, {:error, error}}
          end
        end)
      end
    end
  end

  defp add_relationship_to_changeset(changeset, relationship, relationship_data) do
    IO.inspect(relationship, label: "relationship")
    IO.inspect(relationship_data, label: "relationship data")

    {:ok, changeset}
  end

  # defp do_authorization_changeset(changeset, []) do
  #   changeset
  # end

  # defp do_authorization_changeset(
  #        changeset,
  #        relationships_being_changed
  #      ) do
  #   fn data ->
  #     Enum.reduce(relationships_being_changed, changeset, fn relationship, changeset ->

  #     end)
  #     # changeset
  #     # changeset
  #     # |> update_relationship_fields(data, relationships_needing_field_update)
  #     # |> split_relationships_being_replaced(data, relationships_being_replaced)
  #   end
  # end

  # defp update_relationship_fields(changeset, data, relationships_needing_field_update) do
  #   Enum.reduce(relationships_needing_field_update, changeset, fn relationship, changeset ->
  #     related = get_in(data, [:relationships, relationship.name, :to_add])

  #     if related do
  #       value = Map.get(related, relationship.destination_field)
  #       Ecto.Changeset.cast(changeset, relationship.source_field, value)
  #     else
  #       Ecto.Changeset.add_error(changeset, relationship.name, "Invalid relationship data")
  #     end
  #   end)
  # end

  # defp split_relationships_being_replaced(changeset, data, relationships_being_replaced) do
  #   # TODO: this
  #   changeset
  # end

  defp fetch_string_or_atom(map, name) do
    case Map.fetch(map, name) do
      {:ok, value} -> {:ok, value}
      :error -> Map.fetch(map, to_string(name))
    end
  end

  # defp read_to_relate_authorization(
  #        api,
  #        %{destination: destination} = relationship,
  #        value
  #      ) do
  #   default_read =
  #     Ash.primary_action(destination, :read) ||
  #       raise "Need a default read action for #{destination}"

  #   relationship_name = relationship.name

  #   filter =
  #     case relationship.cardinality do
  #       :many ->
  #         case value do
  #           [single_identifier] ->
  #             single_identifier

  #           many ->
  #             [or: many]
  #         end

  #       :one ->
  #         value
  #     end

  #   Ash.Authorization.Request.new(
  #     api: api,
  #     authorization_steps: default_read.authorization_steps,
  #     resource: relationship.destination,
  #     action_type: :read,
  #     filter: filter,
  #     must_fetch?: false,
  #     state_key: [:relationships, relationship_name, :to_relate],
  #     fetcher: fn _ ->
  #       case api.read(destination, filter: filter, paginate: false) do
  #         {:ok, %{results: results}} -> {:ok, results}
  #         {:error, error} -> {:error, error}
  #       end
  #     end,
  #     relationship: [relationship.name],
  #     source: "read prior to write related #{relationship.name}"
  #   )
  # end

  # defp identifier_or_identifiers(relationship, data) do
  #   case relationship.cardinality do
  #     :many ->
  #       Ash.Actions.PrimaryKeyHelpers.values_to_primary_key_filters(
  #         relationship.destination,
  #         data
  #       )

  #     :one ->
  #       Ash.Actions.PrimaryKeyHelpers.value_to_primary_key_filter(
  #         relationship.destination,
  #         data
  #       )
  #   end
  # end

  # defp add_update_replace_authorizations(api, action, relationship, data, changeset) do
  #   case identifier_or_identifiers(relationship, data) do
  #     {:ok, identifiers} ->
  #       read_currently_related_authorization =
  #         read_currently_related_authorization(api, changeset, relationship)

  #       read_authorization =
  #         read_related_authorization(api, relationship, identifiers, :to_replace)

  #       changeset
  #       |> add_to_relationship_change_metadata(relationship, :replace, identifiers)
  #       |> add_authorizations(read_currently_related_authorization)
  #       |> add_authorizations(read_authorization)
  #       |> changes_depend_on([:relationships, relationship.name, :replace_result])

  #     {:error, error} ->
  #       {:error, error}
  #   end
  # end

  # defp to_replace_relationship_authorization(api, %{source: resource} = relationship, identifiers) do
  #   # TODO: This needs to have a state_key of `replace_result`, and this should look
  #   # at the `current` state for the relationship, and return %{add: add, replace: replace}
  #   # then in `authorization_changeset` we go through all relationships being replaced,
  #   # and read their `replace_results` and put them into the authorization changeset.
  #   # This also needs to make the required changes!
  #   Ash.Authorization.Request.new(
  #     api: api,
  #     authorization_steps: _read.authorization_steps,
  #     resource: resource,
  #     action_type: :update
  #   )
  # end

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
        authorization_steps: default_read.authorization_steps,
        resource: destination,
        action_type: :read,
        state_key: [:relationships, relationship.name, :current],
        must_fetch?: true,
        filter: filter,
        fetcher: fn _ ->
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

  # defp add_create_authorizations(api, relationship, data, changeset) do
  #   case identifier_or_identifiers(relationship, data) do
  #     {:ok, identifiers} ->
  #       read_authorization = read_related_authorization(api, relationship, identifiers, :to_add)

  #       changeset
  #       |> add_to_relationship_change_metadata(relationship, :add, identifiers)
  #       |> add_authorizations(read_authorization)
  #       |> add_authorizations(add_authorization)
  #       |> set_belongs_to_change(relationship, identifiers)

  #     {:error, error} ->
  #       {:error, error}
  #   end
  # end

  # defp add_to_relationship_change_metadata(changeset, relationship, key, identifiers) do
  #   changeset
  #   |> Map.put_new(:__ash_relationships__, %{})
  #   |> Map.update!(:__ash_relationships__, fn ash_relationships ->
  #     ash_relationships
  #     |> Map.put_new(relationship.name, %{})
  #     |> Map.update!(relationship.name, fn changes -> Map.put(changes, key, identifiers) end)
  #   end)
  # end

  # defp set_belongs_to_change(
  #        changeset,
  #        %{
  #          type: :belongs_to,
  #          source_field: source_field,
  #          name: name,
  #          destination_field: destination_field
  #        },
  #        value
  #      ) do
  #   if Keyword.has_key?(value, destination_field) do
  #     changeset
  #     |> Ecto.Changeset.put_change(
  #       source_field,
  #       Keyword.fetch!(value, destination_field)
  #     )
  #     |> Map.put_new(:__ash_skip_authorization_fields__, [])
  #     |> Map.update!(:__ash_skip_authorization_fields__, fn fields ->
  #       [source_field | fields]
  #     end)
  #   else
  #     changes_depend_on(changeset, [:relationships, name, :to_add])
  #   end
  # end

  # defp set_belongs_to_change(changeset, _, _) do
  #   changeset
  # end

  defp changes_depend_on(changeset, path) do
    Map.update(changeset, :__changes_depend_on__, [path], fn paths -> [path | paths] end)
  end

  defp add_authorizations(changeset, authorizations) do
    authorizations = List.wrap(authorizations)
    Map.update(changeset, :__authorizations__, authorizations, &Kernel.++(&1, authorizations))
  end

  # defp add_to_relationship_authorization(
  #        api,
  #        identifier,
  #        %{type: :has_one, name: name, destination: destination} = relationship,
  #        changeset
  #      ) do
  #   pkey = Ash.primary_key(destination)

  #   Ash.Authorization.Request.new(
  #     api: api,
  #     authorization_steps: relationship.authorization_steps,
  #     resource: relationship.source,
  #     changeset: changeset,
  #     action_type: :create,
  #     state_key: :data,
  #     must_fetch?: true,
  #     dependencies: [:data, [:relationships, name, :to_add]],
  #     is_fetched: fn data ->
  #       case data do
  #         %{^name => %Ecto.Association.NotLoaded{}} -> false
  #         _ -> true
  #       end
  #     end,
  #     fetcher: fn %{data: data, relationships: %{^name => %{:to_add => to_add}}} ->
  #       pkey_value = Keyword.take(identifier, pkey)

  #       related =
  #         Enum.find(to_add, fn to_relate ->
  #           to_relate
  #           |> Map.take(pkey)
  #           |> Map.to_list()
  #           |> Kernel.==(pkey_value)
  #         end)

  #       updated =
  #         api.update(related,
  #           attributes: %{
  #             relationship.destination_field => Map.get(data, relationship.source_field)
  #           }
  #         )

  #       case updated do
  #         {:ok, updated} -> {:ok, Map.put(data, relationship.name, updated)}
  #         {:error, error} -> {:error, error}
  #       end
  #     end,
  #     relationship: [],
  #     bypass_strict_access?: true,
  #     source: "Update relationship #{name}"
  #   )
  # end

  # defp add_to_relationship_authorization(
  #        api,
  #        _identifier,
  #        %{type: :belongs_to, name: name} = relationship,
  #        changeset
  #      ) do
  #   Ash.Authorization.Request.new(
  #     api: api,
  #     authorization_steps: relationship.authorization_steps,
  #     resource: relationship.source,
  #     action_type: :update,
  #     state_key: :data,
  #     is_fetched: fn data ->
  #       case data do
  #         %{^name => %Ecto.Association.NotLoaded{}} -> false
  #         _ -> true
  #       end
  #     end,
  #     must_fetch?: true,
  #     dependencies: [:data, [:relationships, name, :to_add]],
  #     bypass_strict_access?: true,
  #     changeset: changeset,
  #     fetcher: fn %{data: data, relationships: %{^name => %{:to_add => to_add}}} ->
  #       case to_add do
  #         [item] -> {:ok, Map.put(data, name, item)}
  #         _ -> raise "Internal relationship assumption failed."
  #       end
  #     end,
  #     relationship: [],
  #     source: "Set relationship #{relationship.name}"
  #   )
  # end

  # defp add_to_relationship_authorization(
  #        api,
  #        identifiers,
  #        %{destination: destination, name: name, type: :many_to_many} = relationship,
  #        changeset
  #      ) do
  #   Enum.flat_map(identifiers, fn identifier ->
  #     pkey = Ash.primary_key(destination)

  #     default_create =
  #       Ash.primary_action(relationship.through, :create) ||
  #         raise "Must define a default create action for #{relationship.through}"

  #     [
  #       Ash.Authorization.Request.new(
  #         api: api,
  #         authorization_steps: default_create.authorization_steps,
  #         resource: relationship.through,
  #         changeset: fn %{data: data, relationships: %{^name => %{to_add: to_add}}} ->
  #           pkey_value = Keyword.take(identifier, pkey)

  #           related =
  #             Enum.find(to_add, fn to_relate ->
  #               to_relate
  #               |> Map.take(pkey)
  #               |> Map.to_list()
  #               |> Kernel.==(pkey_value)
  #             end)

  #           attributes = %{
  #             relationship.destination_field_on_join_table =>
  #               Map.fetch!(related, relationship.destination_field_on_join_table),
  #             relationship.source_field_on_join_table =>
  #               Map.fetch!(data, relationship.source_field_on_join_table)
  #           }

  #           changeset = Ash.Actions.Create.changeset(api, relationship.through, attributes)

  #           if changeset.valid? do
  #             {:ok, changeset}
  #           else
  #             {:error, changeset}
  #           end
  #         end,
  #         action_type: :create,
  #         state_key: [:relationships, name, :created_join_table_rows],
  #         must_fetch?: true,
  #         dependencies: [[:relationships, name, :to_add], :data],
  #         fetcher: fn %{data: data, relationships: %{^name => %{to_add: to_add}}} ->
  #           pkey_value = Keyword.take(identifier, pkey)

  #           related =
  #             Enum.find(to_add, fn to_relate ->
  #               to_relate
  #               |> Map.take(pkey)
  #               |> Map.to_list()
  #               |> Kernel.==(pkey_value)
  #             end)

  #           attributes = %{
  #             relationship.destination_field_on_join_table =>
  #               Map.fetch!(related, relationship.destination_field),
  #             relationship.source_field_on_join_table =>
  #               Map.fetch!(data, relationship.source_field)
  #           }

  #           api.create(relationship.through, attributes: attributes)
  #         end,
  #         relationship: [],
  #         bypass_strict_access?: true,
  #         source: "Create join entry for relationship #{name}"
  #       ),
  #       Ash.Authorization.Request.new(
  #         api: api,
  #         authorization_steps: relationship.authorization_steps,
  #         resource: relationship.source,
  #         changeset: changeset,
  #         action_type: :create,
  #         state_key: :data,
  #         must_fetch?: true,
  #         dependencies: [[:relationships, name, :to_add], :data],
  #         is_fetched: fn data ->
  #           case Map.get(data, name) do
  #             %Ecto.Association.NotLoaded{} ->
  #               false

  #             related ->
  #               Enum.any?(related, fn related ->
  #                 related
  #                 |> Map.take(pkey)
  #                 |> Map.to_list()
  #                 |> Kernel.==(identifier)
  #               end)
  #           end
  #         end,
  #         fetcher: fn %{data: data, relationships: %{^name => %{to_add: to_add}}} ->
  #           pkey_value = Keyword.take(identifier, pkey)

  #           related =
  #             Enum.find(to_add, fn to_relate ->
  #               to_relate
  #               |> Map.take(pkey)
  #               |> Map.to_list()
  #               |> Kernel.==(pkey_value)
  #             end)

  #           data_with_related =
  #             Map.update!(data, name, fn
  #               %Ecto.Association.NotLoaded{} ->
  #                 [related]

  #               items ->
  #                 items ++ [related]
  #             end)

  #           {:ok, data_with_related}
  #         end,
  #         relationship: [],
  #         bypass_strict_access?: true,
  #         source: "Update relationship #{name}"
  #       )
  #     ]
  #   end)
  # end

  # defp add_to_relationship_authorization(
  #        api,
  #        identifiers,
  #        %{destination: destination, name: name, type: :has_many} = relationship,
  #        changeset
  #      ) do
  #   Enum.flat_map(identifiers, fn identifier ->
  #     pkey = Ash.primary_key(destination)

  #     [
  #       Ash.Authorization.Request.new(
  #         api: api,
  #         authorization_steps: relationship.authorization_steps,
  #         resource: relationship.source,
  #         changeset: changeset,
  #         action_type: :create,
  #         state_key: :data,
  #         must_fetch?: true,
  #         dependencies: [[:relationships, name, :to_add], :data],
  #         is_fetched: fn data ->
  #           case Map.get(data, name) do
  #             %Ecto.Association.NotLoaded{} ->
  #               false

  #             related ->
  #               Enum.any?(related, fn related ->
  #                 related
  #                 |> Map.take(pkey)
  #                 |> Map.to_list()
  #                 |> Kernel.==(identifier)
  #               end)
  #           end
  #         end,
  #         fetcher: fn %{data: data, relationships: %{^name => %{to_add: to_add}}} ->
  #           pkey_value = Keyword.take(identifier, pkey)

  #           related =
  #             Enum.find(to_add, fn to_relate ->
  #               to_relate
  #               |> Map.take(pkey)
  #               |> Map.to_list()
  #               |> Kernel.==(pkey_value)
  #             end)

  #           updated =
  #             api.update(related,
  #               attributes: %{
  #                 relationship.destination_field => Map.get(data, relationship.source_field)
  #               }
  #             )

  #           case updated do
  #             {:ok, updated} ->
  #               updated_with_related =
  #                 Map.update!(data, name, fn
  #                   %Ecto.Association.NotLoaded{} ->
  #                     [updated]

  #                   items ->
  #                     items ++ [updated]
  #                 end)

  #               {:ok, updated_with_related}

  #             {:error, error} ->
  #               {:error, error}
  #           end
  #         end,
  #         relationship: [],
  #         bypass_strict_access?: true,
  #         source: "Update relationship #{name}"
  #       )
  #     ]
  #   end)
  # end
end

# def handle_update_relationships(changeset, api, relationships_input) do
#   Enum.reduce(relationships_input, changeset, fn {name, data}, changeset ->
#     case Ash.relationship(changeset.data.__struct__, name) do
#       nil ->
#         Ecto.Changeset.add_error(changeset, name, "Invalid relationship")

#       relationship ->
#         cond do
#           !is_map(data) ->
#             add_update_replace_authorizations(api, relationship, data, changeset)

#           keys_are?(data, [:add, :remove]) ->
#             raise "uh oh, not there yet! 1"

#           # add needs to take an action type so we can authorize it properly

#           # changeset = add_update_add_authorizations(api, relationship, data, changeset)
#           # add_update_remove_authorizations(api, relationships, data, changeset)
#           keys_are?(data, [:add]) ->
#             raise "uh oh, not there yet! 2"

#           # add_update_add_authorizations(api, relationship, data, changeset)
#           keys_are?(data, [:remove]) ->
#             raise "uh oh, not there yet! 3"

#           # add_update_remove_authorizations(api, relationships, data, changeset)
#           keys_are?(data, [:replace]) ->
#             add_update_replace_authorizations(api, relationship, data, changeset)

#           keys_are?(data, [:add, :replace]) ->
#             Ecto.Changeset.add_error(
#               changeset,
#               relationship.name,
#               "Cannot add to a relationship and replace it at the same time."
#             )

#           true ->
#             Ecto.Changeset.add_error(
#               changeset,
#               relationship.name,
#               "Invalid relationship data provided 1"
#             )
#         end
#     end
#   end)
# end

# defp keys_are?(keyword, keys) do
#   Enum.sort(Map.keys(keyword)) == Enum.sort(keys)
# end

# def handle_create_relationships(changeset, api, relationships_input) do
#   Enum.reduce(relationships_input, changeset, fn {name, data}, changeset ->
#     case Ash.relationship(changeset.data.__struct__, name) do
#       nil ->
#         Ecto.Changeset.add_error(changeset, name, "Invalid relationship")

#       relationship ->
#         cond do
#           !is_map(data) ->
#             add_create_authorizations(api, relationship, data, changeset)

#           keys_are?(data, [:add]) ->
#             add_create_authorizations(api, relationship, data[:add], changeset)

#           keys_are?(data, [:replace]) ->
#             add_create_authorizations(api, relationship, data[:replace], changeset)

#           keys_are?(data, [:add, :replace]) ->
#             Ecto.Changeset.add_error(
#               changeset,
#               relationship.name,
#               "Cannot add to a relationship and replace it at the same time."
#             )

#           Keyword.has_key?(data, :remove) ->
#             Ecto.Changeset.add_error(
#               changeset,
#               relationship.name,
#               "Cannot remove from a relationship on create."
#             )

#           true ->
#             Ecto.Changeset.add_error(
#               changeset,
#               relationship.name,
#               "Invalid relationship data provided 2"
#             )
#         end
#     end
#   end)
# end
