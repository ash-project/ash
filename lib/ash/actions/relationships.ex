defmodule Ash.Actions.Relationships do
  def handle_create_relationships(changeset, api, relationships_input) do
    Enum.reduce(relationships_input, changeset, fn {name, data}, changeset ->
      case Ash.relationship(changeset.data.__struct__, name) do
        nil ->
          Ecto.Changeset.add_error(changeset, name, "Invalid relationship")

        relationship ->
          cond do
            !Keyword.keyword?(data) ->
              add_create_authorizations(api, relationship, data, changeset)

            Keyword.keys(data) == [:add] ->
              add_create_authorizations(api, relationship, data[:add], changeset)

            Keyword.keys(data) == [:replace] ->
              add_create_authorizations(api, relationship, data[:replace], changeset)

            Keyword.keys(data) == [:add, :replace] ->
              Ecto.Changeset.add_error(
                changeset,
                relationship.name,
                "Cannot add to a relationship and replace it at the same time."
              )

            Keyword.has_key?(data, :remove) ->
              Ecto.Changeset.add_error(
                changeset,
                relationship.name,
                "Cannot remove from a relationship on create."
              )

            true ->
              Ecto.Changeset.add_error(
                changeset,
                relationship.name,
                "Invalid relationship data provided"
              )
          end
      end
    end)
  end

  def authorization_changeset_with_foreign_keys(changeset, relationships) do
    relationships
    |> Enum.reduce_while({:ok, []}, fn {name, data}, {:ok, relationships} ->
      case Ash.relationship(changeset.data.__struct__, name) do
        nil ->
          {:halt, {:error, name}}

        %{type: :belongs_to, destination_field: destination_field} = relationship ->
          case identifier_or_identifiers(relationship, data) do
            {:ok, identifier} ->
              if Keyword.has_key?(identifier, destination_field) do
                {:cont, {:ok, relationships}}
              else
                {:cont, {:ok, [relationship | relationships]}}
              end

            {:error, _} ->
              {:halt, {:error, name}}
          end

        _ ->
          {:cont, {:ok, relationships}}
      end
    end)
    |> case do
      {:error, name} ->
        Ecto.Changeset.add_error(changeset, name, "Invalid relationship")

      {:ok, []} ->
        changeset

      {:ok, relationships_needing_fetch} ->
        fn data ->
          Enum.reduce(relationships_needing_fetch, changeset, fn relationship, changeset ->
            related = get_in(data, [:relationships, relationship.name, :to_add])

            if related do
              value = Map.get(related, relationship.destination_field)
              Ecto.Changeset.cast(changeset, relationship.source_field, value)
            else
              Ecto.Changeset.add_error(changeset, relationship.name, "Invalid relationship data")
            end
          end)
        end
    end
  end

  defp read_related_authorization(
         api,
         %{destination: destination} = relationship,
         value,
         key
       ) do
    default_read =
      Ash.primary_action(destination, :read) ||
        raise "Need a default read action for #{destination}"

    relationship_name = relationship.name

    filter =
      case relationship.cardinality do
        :many ->
          [or: value]

        :one ->
          value
      end

    Ash.Authorization.Request.new(
      api: api,
      authorization_steps: default_read.authorization_steps,
      resource: relationship.destination,
      action_type: :read,
      filter: filter,
      must_fetch?: true,
      state_key: [:relationships, relationship_name, key],
      fetcher: fn ->
        case api.read(destination, filter: filter, paginate: false) do
          {:ok, %{results: results}} -> {:ok, results}
          {:error, error} -> {:error, error}
        end
      end,
      relationship: [relationship.name],
      source: "read prior to write related #{relationship.name}"
    )
  end

  defp identifier_or_identifiers(relationship, data) do
    case relationship.cardinality do
      :many ->
        Ash.Actions.PrimaryKeyHelpers.values_to_primary_key_filters(
          relationship.destination,
          data
        )

      :one ->
        Ash.Actions.PrimaryKeyHelpers.value_to_primary_key_filter(
          relationship.destination,
          data
        )
    end
  end

  defp add_create_authorizations(api, relationship, data, changeset) do
    case identifier_or_identifiers(relationship, data) do
      {:ok, identifiers} ->
        changeset =
          add_to_relationship_change_metadata(changeset, relationship, :add, identifiers)

        read_authorization = read_related_authorization(api, relationship, identifiers, :to_add)

        add_authorization =
          add_to_relationship_authorization(api, identifiers, relationship, changeset)

        changeset
        |> add_authorizations(read_authorization)
        |> add_authorizations(add_authorization)
        |> set_belongs_to_change(relationship, identifiers)

      {:error, error} ->
        {:error, error}
    end
  end

  defp add_to_relationship_change_metadata(changeset, relationship, key, identifiers) do
    changeset
    |> Map.put_new(:__ash_relationships__, %{})
    |> Map.update!(:__ash_relationships__, fn ash_relationships ->
      ash_relationships
      |> Map.put_new(relationship.name, %{})
      |> Map.update!(relationship.name, fn changes -> Map.put(changes, key, identifiers) end)
    end)
  end

  defp set_belongs_to_change(
         changeset,
         %{
           type: :belongs_to,
           source_field: source_field,
           name: name,
           destination_field: destination_field
         },
         value
       ) do
    if Keyword.has_key?(value, destination_field) do
      changeset
      |> Ecto.Changeset.put_change(
        source_field,
        Keyword.fetch!(value, destination_field)
      )
      |> Map.put_new(:__ash_skip_authorization_fields__, [])
      |> Map.update!(:__ash_skip_authorization_fields__, fn fields ->
        [source_field | fields]
      end)
    else
      Map.put_new(changeset, :__changes_depend_on__, [:relationships, name, :to_add])
    end
  end

  defp set_belongs_to_change(changeset, _, _) do
    changeset
  end

  defp add_authorizations(changeset, authorizations) do
    authorizations = List.wrap(authorizations)
    Map.update(changeset, :__authorizations__, authorizations, &Kernel.++(&1, authorizations))
  end

  defp add_to_relationship_authorization(
         api,
         identifier,
         %{type: :has_one, name: name, destination: destination} = relationship,
         changeset
       ) do
    pkey = Ash.primary_key(destination)

    Ash.Authorization.Request.new(
      api: api,
      authorization_steps: relationship.authorization_steps,
      resource: relationship.source,
      changeset: changeset,
      action_type: :create,
      state_key: :data,
      must_fetch?: true,
      dependencies: [:data, [:relationships, name, :to_add]],
      is_fetched: fn data ->
        case data do
          %{^name => %Ecto.Association.NotLoaded{}} -> false
          _ -> true
        end
      end,
      fetcher: fn %{data: data, relationships: %{^name => %{:to_add => to_add}}} ->
        pkey_value = Keyword.take(identifier, pkey)

        related =
          Enum.find(to_add, fn to_relate ->
            to_relate
            |> Map.take(pkey)
            |> Map.to_list()
            |> Kernel.==(pkey_value)
          end)

        updated =
          api.update(related,
            attributes: %{
              relationship.destination_field => Map.get(data, relationship.source_field)
            }
          )

        case updated do
          {:ok, updated} -> {:ok, Map.put(data, relationship.name, updated)}
          {:error, error} -> {:error, error}
        end
      end,
      relationship: [],
      bypass_strict_access?: true,
      source: "Update relationship #{name}"
    )
  end

  defp add_to_relationship_authorization(
         api,
         _identifier,
         %{type: :belongs_to, name: name} = relationship,
         changeset
       ) do
    Ash.Authorization.Request.new(
      api: api,
      authorization_steps: relationship.authorization_steps,
      resource: relationship.source,
      action_type: :update,
      state_key: :data,
      is_fetched: fn data ->
        case data do
          %{^name => %Ecto.Association.NotLoaded{}} -> false
          _ -> true
        end
      end,
      must_fetch?: true,
      dependencies: [:data, [:relationships, name, :to_add]],
      bypass_strict_access?: true,
      changeset: changeset,
      fetcher: fn %{data: data, relationships: %{^name => %{:to_add => to_add}}} ->
        case to_add do
          [item] -> {:ok, Map.put(data, name, item)}
          _ -> raise "Internal relationship assumption failed."
        end
      end,
      relationship: [],
      source: "Set relationship #{relationship.name}"
    )
  end

  defp add_to_relationship_authorization(
         api,
         identifiers,
         %{destination: destination, name: name, type: :many_to_many} = relationship,
         changeset
       ) do
    Enum.flat_map(identifiers, fn identifier ->
      pkey = Ash.primary_key(destination)

      default_create =
        Ash.primary_action(relationship.through, :create) ||
          raise "Must define a default create action for #{relationship.through}"

      [
        Ash.Authorization.Request.new(
          api: api,
          authorization_steps: default_create.authorization_steps,
          resource: relationship.through,
          changeset: fn %{data: data, relationships: %{^name => %{to_add: to_add}}} ->
            pkey_value = Keyword.take(identifier, pkey)

            related =
              Enum.find(to_add, fn to_relate ->
                to_relate
                |> Map.take(pkey)
                |> Map.to_list()
                |> Kernel.==(pkey_value)
              end)

            attributes = %{
              relationship.destination_field_on_join_table =>
                Map.fetch!(related, relationship.destination_field_on_join_table),
              relationship.source_field_on_join_table =>
                Map.fetch!(data, relationship.source_field_on_join_table)
            }

            changeset = Ash.Actions.Create.changeset(api, relationship.through, attributes)

            if changeset.valid? do
              {:ok, changeset}
            else
              {:error, changeset}
            end
          end,
          action_type: :create,
          state_key: [:relationships, name, :created_join_table_rows],
          must_fetch?: true,
          dependencies: [[:relationships, name, :to_add], :data],
          fetcher: fn %{data: data, relationships: %{^name => %{to_add: to_add}}} ->
            pkey_value = Keyword.take(identifier, pkey)

            related =
              Enum.find(to_add, fn to_relate ->
                to_relate
                |> Map.take(pkey)
                |> Map.to_list()
                |> Kernel.==(pkey_value)
              end)

            attributes = %{
              relationship.destination_field_on_join_table =>
                Map.fetch!(related, relationship.destination_field),
              relationship.source_field_on_join_table =>
                Map.fetch!(data, relationship.source_field)
            }

            api.create(relationship.through, attributes: attributes)
          end,
          relationship: [],
          bypass_strict_access?: true,
          source: "Create join entry for relationship #{name}"
        ),
        Ash.Authorization.Request.new(
          api: api,
          authorization_steps: relationship.authorization_steps,
          resource: relationship.source,
          changeset: changeset,
          action_type: :create,
          state_key: :data,
          must_fetch?: true,
          dependencies: [[:relationships, name, :to_add], :data],
          is_fetched: fn data ->
            case Map.get(data, name) do
              %Ecto.Association.NotLoaded{} ->
                false

              related ->
                Enum.any?(related, fn related ->
                  related
                  |> Map.take(pkey)
                  |> Map.to_list()
                  |> Kernel.==(identifier)
                end)
            end
          end,
          fetcher: fn %{data: data, relationships: %{^name => %{to_add: to_add}}} ->
            pkey_value = Keyword.take(identifier, pkey)

            related =
              Enum.find(to_add, fn to_relate ->
                to_relate
                |> Map.take(pkey)
                |> Map.to_list()
                |> Kernel.==(pkey_value)
              end)

            data_with_related =
              Map.update!(data, name, fn
                %Ecto.Association.NotLoaded{} ->
                  [related]

                items ->
                  items ++ [related]
              end)

            {:ok, data_with_related}
          end,
          relationship: [],
          bypass_strict_access?: true,
          source: "Update relationship #{name}"
        )
      ]
    end)
  end

  defp add_to_relationship_authorization(
         api,
         identifiers,
         %{destination: destination, name: name, type: :has_many} = relationship,
         changeset
       ) do
    Enum.flat_map(identifiers, fn identifier ->
      pkey = Ash.primary_key(destination)

      [
        Ash.Authorization.Request.new(
          api: api,
          authorization_steps: relationship.authorization_steps,
          resource: relationship.source,
          changeset: changeset,
          action_type: :create,
          state_key: :data,
          must_fetch?: true,
          dependencies: [[:relationships, name, :to_add], :data],
          is_fetched: fn data ->
            case Map.get(data, name) do
              %Ecto.Association.NotLoaded{} ->
                false

              related ->
                Enum.any?(related, fn related ->
                  related
                  |> Map.take(pkey)
                  |> Map.to_list()
                  |> Kernel.==(identifier)
                end)
            end
          end,
          fetcher: fn %{data: data, relationships: %{^name => %{to_add: to_add}}} ->
            pkey_value = Keyword.take(identifier, pkey)

            related =
              Enum.find(to_add, fn to_relate ->
                to_relate
                |> Map.take(pkey)
                |> Map.to_list()
                |> Kernel.==(pkey_value)
              end)

            updated =
              api.update(related,
                attributes: %{
                  relationship.destination_field => Map.get(data, relationship.source_field)
                }
              )

            case updated do
              {:ok, updated} ->
                updated_with_related =
                  Map.update!(data, name, fn
                    %Ecto.Association.NotLoaded{} ->
                      [updated]

                    items ->
                      items ++ [updated]
                  end)

                {:ok, updated_with_related}

              {:error, error} ->
                {:error, error}
            end
          end,
          relationship: [],
          bypass_strict_access?: true,
          source: "Update relationship #{name}"
        )
      ]
    end)
  end
end
