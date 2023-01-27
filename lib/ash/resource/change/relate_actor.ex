defmodule Ash.Resource.Change.RelateActor do
  @moduledoc false
  use Ash.Resource.Change
  alias Ash.Changeset
  alias Ash.Error.Changes.InvalidRelationship

  def init(opts) do
    case Spark.OptionsHelpers.validate(opts, Ash.Resource.Change.Builtins.relate_actor_opts()) do
      {:ok, opts} ->
        {:ok, opts}

      {:error, error} ->
        {:error, Exception.message(error)}
    end
  end

  def change(changeset, opts, %{actor: nil}) do
    if opts[:allow_nil?] do
      changeset
    else
      Changeset.add_error(
        changeset,
        InvalidRelationship.exception(
          relationship: opts[:relationship],
          message: "Could not relate to actor, as no actor was found (and :allow_nil? is false)"
        )
      )
    end
  end

  def change(changeset, opts, %{actor: actor}) do
    Changeset.manage_relationship(changeset, opts[:relationship], actor, type: :append_and_remove)
  end
end
