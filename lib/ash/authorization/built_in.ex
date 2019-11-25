defmodule Ash.Authorization.BuiltIn do
  # nil user is handled by precheck
  def owner_relationship(user, data, %{resource: resource, relationship: relationship_name}) do
    relationship = Ash.relationship(resource, relationship_name)

    # The precheck sideloads the relationship
    data
    |> Stream.filter(fn item ->
      item
      |> Map.get(relationship_name)
      |> Kernel.||([])
      |> Enum.find(fn related ->
        Map.get(related, relationship.destination_field) == user.id
      end)
    end)
    |> Enum.map(&Map.get(&1, :id))
  end

  def owner_relationship_precheck(nil, _), do: {:precheck, false}

  def owner_relationship_precheck(user, %{
        relationship: relationship_name,
        params: params,
        resource: resource
      }) do
    relationship = Ash.relationship(resource, relationship_name)
    user_id = user.id
    source_field = relationship.source_field

    cond do
      match?(%{filter: %{^relationship_name => ^user_id}}, params) ->
        # TODO: relationship filters not yet supported, so this won't actually work
        # remove this TODO and double check this functionality when they are
        {:precheck, true}

      relationship.type != :many_to_many &&
          match?(%{filter: %{^source_field => ^user_id}}, params) ->
        {:precheck, true}

      true ->
        {:side_load, relationship_name}
    end
  end
end
