defmodule Ash.Actions.Relationships do
  def relationship_change_authorizations(api, resource, changeset) do
    resource
    |> Ash.relationships()
    |> Enum.filter(fn relationship ->
      Map.has_key?(changeset.__ash_relationships__, relationship.name)
    end)
    |> Enum.flat_map(fn relationship ->
      add_related_authorizations(api, relationship, changeset)
    end)
  end

  defp add_related_authorizations(
         api,
         %{destination: destination} = relationship,
         changeset
       ) do
    default_read =
      Ash.primary_action(destination, :read) ||
        raise "Need a default read action for #{destination}"

    relationship_name = relationship.name

    value =
      changeset.__ash_relationships__
      |> Map.get(relationship_name)
      |> Map.get(:add, [])

    filter =
      case relationship.cardinality do
        :many ->
          [or: value]

        :one ->
          value
      end

    read_request =
      Ash.Authorization.Request.new(
        api: api,
        authorization_steps: default_read.authorization_steps,
        resource: relationship.destination,
        action_type: :read,
        filter: filter,
        must_fetch?: true,
        state_key: [:relationships, relationship_name, :to_add],
        fetcher: fn ->
          case api.read(destination, filter: filter, paginate: false) do
            {:ok, %{results: results}} -> {:ok, results}
            {:error, error} -> {:error, error}
          end
        end,
        relationship: [relationship.name],
        source: "read prior to write related #{relationship.name}"
      )

    related_requests = related_add_authorization_requests(api, value, relationship, changeset)

    [read_request | related_requests]
  end

  defp related_add_authorization_requests(
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

            changeset = Ash.Actions.Create.changeset(relationship.through, attributes)

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

  defp related_add_authorization_requests(
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
                },
                # TODO: This does nothing, but is intended for use when we disallow writing to fields that point to
                # relationships
                system?: true
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

  defp related_add_authorization_requests(
         api,
         _identifier,
         %{type: :belongs_to, name: name} = relationship,
         changeset
       ) do
    [
      Ash.Authorization.Request.new(
        api: api,
        authorization_steps: relationship.authorization_steps,
        resource: relationship.source,
        action_type: :update,
        state_key: :data,
        is_fetched: fn data ->
          Map.get(data, name) != %Ecto.Association.NotLoaded{}
        end,
        must_fetch?: true,
        dependencies: [:data, [:relationships, name, :to_add]],
        bypass_strict_access?: true,
        changeset: changeset,
        fetcher: fn %{data: data, relationships: %{^name => %{:to_add => to_add}}} ->
          {:ok, Map.put(data, name, to_add)}
        end,
        relationship: [],
        source: "Set relationship #{relationship.name}"
      )
    ]
  end

  defp related_add_authorization_requests(
         api,
         identifier,
         %{type: :has_one, name: name, destination: destination} = relationship,
         changeset
       ) do
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
        dependencies: [:data, [:relationships, name, :to_add]],
        is_fetched: fn data ->
          Map.get(data, name) != %Ecto.Association.NotLoaded{}
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
              },
              # TODO: This does nothing, but is intended for use when we disallow writing to fields that point to
              # relationships
              system?: true
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
    ]
  end
end
