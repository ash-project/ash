defmodule Ash.Authorization.Check.RelationshipAccess do
  @moduledoc """
  Allows the user to access the data if they are related to the resource via the provided relationship.

  use `enforce_access?: true` to have the precheck only allow access via the relationship,
  or that relationship's foreign keys.

  #TODO: Document this better
  """
  use Ash.Authorization.Check

  def init(opts) do
    with {:key, {:ok, relationship}} <- {:key, Keyword.fetch(opts, :relationship)},
         {:is_nil, false} <- {:is_nil, is_nil(relationship)},
         {:atom, true} <- {:atom, is_atom(relationship)} do
      {:ok,
       [relationship: relationship, enforce_access?: Keyword.get(opts, :enforce_access?, true)]}
    else
      {:key, :error} ->
        {:error, "Must supply `:relationship` key"}

      {:is_nil, true} ->
        {:error, "`:relationship` must not be nil"}

      {:atom, false} ->
        {:error, "`:relationship` must be an atom"}
    end
  end

  def check(nil, _, _, _), do: false

  def check(user, data, %{resource: resource}, opts) do
    relationship_name = opts[:relationship]
    relationship = Ash.relationship(resource, relationship_name)

    # The precheck sideloads the relationship
    data
    |> Stream.filter(fn item ->
      item
      |> Map.get(relationship_name)
      |> Kernel.||([])
      |> List.wrap()
      |> Enum.find(fn related ->
        Map.get(related, relationship.destination_field) == user.id
      end)
    end)
    |> Enum.map(&Map.get(&1, :id))
  end

  def describe(opts) do
    "the current user is the #{opts[:relationship]}"
  end

  def precheck(nil, _, _), do: {:precheck, false}

  def precheck(
        user,
        %{
          resource: resource,
          changeset: changeset,
          relationships: relationships,
          action: %{type: :create}
        },
        opts
      ) do
    relationship_name = opts[:relationship]
    relationship = Ash.relationship(resource, relationship_name)
    source_field = relationship.source_field

    cond do
      Ecto.Changeset.get_field(changeset, source_field) == user.id ->
        {:precheck, true}

      match?(
        %{^relationship_name => relationship_change_value}
        when not is_nil(relationship_change_value),
        relationships
      ) ->
        related =
          relationships
          |> Map.get(relationship_name)
          |> Enum.find(&(&1.id == user.id))

        {:precheck, !!related}

      opts[:enforce_access?] ->
        {:precheck, false}

      true ->
        :ok
    end
  end

  def precheck(user, %{resource: resource, params: params}, opts) do
    relationship_name = opts[:relationship]
    relationship = Ash.relationship(resource, relationship_name)
    user_id = user.id
    source_field = relationship.source_field

    cond do
      match?(%{filter: %{^relationship_name => ^user_id}}, params) ->
        {:precheck, true}

      relationship.type != :many_to_many &&
          match?(%{filter: %{^source_field => ^user_id}}, params) ->
        {:precheck, true}

      opts[:enforce_access?] ->
        {:precheck, false}

      true ->
        {:side_load, relationship_name}
    end
  end
end
