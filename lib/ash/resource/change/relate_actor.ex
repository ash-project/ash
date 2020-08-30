defmodule Ash.Resource.Change.RelateActor do
  @moduledoc """
  Relates the actor to the data being changed, as the provided relationship.
  """
  use Ash.Resource.Change
  alias Ash.Changeset
  alias Ash.Error.Changes.InvalidRelationship

  def init(opts) do
    case opts[:relationship] do
      nil ->
        {:error, "Relationship is required"}

      relationship when is_atom(relationship) ->
        {:ok, opts}

      relationship ->
        {:error, "Expected an atom for relationship, got: #{inspect(relationship)}"}
    end
  end

  def change(changeset, opts, %{actor: nil}) do
    Changeset.add_error(
      changeset,
      InvalidRelationship.exception(
        relationship: opts[:relationship],
        message: "Could not relate to actor, as no actor was found"
      )
    )
  end

  def change(changeset, opts, %{actor: actor}) do
    Changeset.replace_relationship(changeset, opts[:relationship], actor)
  end
end
