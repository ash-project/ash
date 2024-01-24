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
    validate_type!(changeset, opts)

    if opts[:allow_nil?] do
      changeset
    else
      Changeset.add_error(
        changeset,
        InvalidRelationship.exception(
          relationship: opts[:relationship],
          message: "could not relate to actor, as no actor was found (and :allow_nil? is false)"
        )
      )
    end
  end

  def change(changeset, opts, %{actor: actor}) do
    validate_type!(changeset, opts)
    field = opts[:field]

    Changeset.manage_relationship(
      changeset,
      opts[:relationship],
      actor_or_field(actor, field),
      type: :append_and_remove
    )
  end

  def atomic(changeset, opts, %{actor: actor}) do
    validate_type!(changeset, opts)
    relationship = Ash.Resource.Info.relationship(changeset.resource, opts[:field])

    if relationship.type == :belongs_to do
      {:atomic,
       %{
         relationship.source_attribute => Map.get(actor, relationship.destination_attribute)
       }}
    else
      {:not_atomic, "Can only use `relate_actor` atomically with a belongs_to relationship."}
    end
  end

  defp validate_type!(changeset, opts) do
    case Ash.Resource.Info.relationship(changeset.resource, opts[:relationship]) do
      %{type: type} when type in [:belongs_to, :has_one] ->
        :ok

      %{type: type} ->
        raise ArgumentError, """
        Cannot use `relate_actor` change with relationship of type #{inspect(type)}.

        It can only be used with a `:belongs_to` or `:has_one` relationship. If you would like to
        add the actor to a list, or something else along those lines, use a custom change
        along with `Ash.Changeset.manage_relationship`.
        """
    end
  end

  defp actor_or_field(actor, field) when is_nil(field) do
    actor
  end

  defp actor_or_field(actor, field) do
    Map.get(actor, field)
  end
end
