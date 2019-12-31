defmodule Ash.Actions.Relationships do
  alias Ash.Actions.PrimaryKeyHelpers

  def relationship_change_authorizations(api, resource, changeset) do
    resource
    |> Ash.relationships()
    |> Enum.filter(fn relationship ->
      Map.has_key?(changeset.__ash_relationships__, relationship.name)
    end)
    |> Enum.reduce_while({:ok, []}, fn relationship, {:ok, authorizations} ->
      case add_related_authorizations(resource, api, relationship, changeset) do
        {:ok, new_authorizations} -> {:cont, {:ok, authorizations ++ new_authorizations}}
        {:error, error} -> {:halt, {:error, error}}
      end
    end)
  end

  def add_relationships_to_result(resource, result, state) do
    state
    |> Map.get(:relationships, %{})
    |> Enum.reduce(result, fn {name, value}, result ->
      # TODO: Figure out `to_remove`
      # how does that look for has_one?
      case Map.fetch(value, :to_add) do
        {:ok, to_add} ->
          case Ash.relationship(resource, name) do
            %{cardinality: :many} ->
              Map.put(result, name, Map.keys(to_add))

            %{cardinality: :one} ->
              Map.put(result, name, to_add)
          end
      end
    end)
  end

  defp wrap_in_list(list) do
    if Keyword.keyword?(list) do
      [list]
    else
      List.wrap(list)
    end
  end

  defp add_related_authorizations(
         resource,
         api,
         %{destination: destination} = relationship,
         changeset
       ) do
    default_read = Ash.primary_action(resource, :read) || raise "Need a default read action for #{resource}"
    relationship_name = relationship.name

    changeset.__ash_relationships__
    |> Map.get(relationship_name)
    |> Map.get(:add, [])
    |> wrap_in_list()
    |> Enum.reduce_while({:ok, []}, fn related_read, {:ok, authorizations} ->
      with {:ok, filters} <-
             PrimaryKeyHelpers.values_to_primary_key_filters(
               destination,
               wrap_in_list(related_read)
             ),
           %{errors: []} = filter <- Ash.Filter.parse(destination, [or: filters], api) do
        read_request =
          Ash.Authorization.Request.new(
            api: api,
            authorization_steps: default_read.authorization_steps,
            resource: relationship.destination,
            action_type: :read,
            filter: filter,
            state_key: [:relationships, relationship_name, :to_add],
            fetcher: fn ->
              api.read(destination, filter: filter)
            end,
            relationship: [relationship.name],
            source: "read prior to write related #{relationship.name}"
          )

        related_requests =
          related_add_authorization_requests(api, related_read, relationship, changeset)

        {:cont, {:ok, [read_request | authorizations] ++ related_requests}}
      else
        {:error, error} -> {:halt, {:error, error}}
      %{errors: errors} -> {:halt, {:error, errors}}
      end
    end)
  end

  defp related_add_authorization_requests(
         api,
         identifier,
         %{destination: destination, name: name, type: :has_many} = relationship,
         changeset
       ) do
    pkey = Ash.primary_key(destination)
    default_update = Ash.primary_action(destination, :update)

    [
      Ash.Authorization.Request.new(
        api: api,
        authorization_steps: relationship.authorization_steps,
        resource: relationship.source,
        changeset: changeset,
        action_type: :create,
        state_key: [:data],
        depends_on: [:data],
        fetcher: fn %{data: data} -> data end,
        relationship: [],
        bypass_strict_access?: true,
        source: "Update relationship #{name}"
      ),
      Ash.Authorization.Request.new(
        api: api,
        authorization_steps: default_update.authorization_steps,
        resource: relationship.destinion,
        action_type: :update,
        state_key: [:relationships, relationship.name, Map.take(identifier, pkey)],
        bypass_strict_access?: true,
        dependencies: [[:relationships, name, :to_add], :data],
        changeset: fn %{data: data, relationships: %{^name => %{:to_add => to_add}}} ->
          related =
            Enum.find(to_add, fn to_relate ->
              Map.take(to_relate, pkey) == Map.take(identifier, pkey)
            end)

          {:ok,
           Ecto.Changeset.cast(
             related,
             %{
               relationship.destination_field => Map.get(data, relationship.source_field)
             },
             [relationship.destination_field]
           )}
        end,
        fetcher: fn %{data: data, relationships: %{^name => %{:to_add => to_add}}} ->
          related =
            Enum.find(to_add, fn to_relate ->
              Map.take(to_relate, pkey) == Map.take(identifier, pkey)
            end)

          api.update(related, %{
            relationship.destination_field => Map.get(data, relationship.source_field)
          })
        end,
        relationship: [relationship.name],
        source: "Update related #{name} from create"
      )
    ]
  end

  defp related_add_authorization_requests(
         api,
         identifier,
         %{type: :belongs_to} = relationship,
         changeset
       ) do
    [
      Ash.Authorization.Request.new(
        api: api,
        authorization_steps: relationship.authorization_steps,
        resource: relationship.source,
        action_type: :update,
        state_key: :data,
        dependencies: [:data],
        bypass_strict_access?: true,
        changeset:
          Ecto.Changeset.put_change(
            changeset,
            relationship.source_field,
            Keyword.get(identifier, relationship.destination_field)
          ),
        fetcher: fn %{data: data} ->
          data
        end,
        relationship: [],
        source: "Set relationship #{relationship.name}"
      )
    ]
  end
end
