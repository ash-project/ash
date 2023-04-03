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
    field = opts[:field]

    Changeset.manage_relationship(
      changeset,
      opts[:relationship],
      actor_or_field(actor, field),
      type: :append_and_remove
    )
  end

  defp actor_or_field(actor, field) when is_nil(field) do
    actor
  end

  defp actor_or_field(actor, field) do
    Map.get(actor, field)
  end
end
