defmodule Ash.Resource.Change.RelateActor do
  @moduledoc false
  use Ash.Resource.Change
  alias Ash.Changeset
  alias Ash.Error.Changes.InvalidRelationship

  @impl true
  def init(opts) do
    case Spark.OptionsHelpers.validate(opts, Ash.Resource.Change.Builtins.relate_actor_opts()) do
      {:ok, opts} ->
        {:ok, opts}

      {:error, error} ->
        {:error, Exception.message(error)}
    end
  end

  @impl true
  def change(changeset, opts, %{actor: actor}) do
    relationship = resolve_relationship!(changeset, opts)
    actor = resolve_actor(actor, opts)
    allow_nil = opts[:allow_nil?]

    case actor do
      nil when allow_nil ->
        changeset

      nil ->
        Changeset.add_error(
          changeset,
          InvalidRelationship.exception(
            relationship: relationship.name,
            message: "could not relate to actor, as no actor was found (and :allow_nil? is false)"
          )
        )

      actor ->
        Changeset.manage_relationship(
          changeset,
          relationship.name,
          actor,
          type: :append_and_remove
        )
    end
  end

  @impl true
  def atomic(changeset, opts, %{actor: actor}) do
    relationship = resolve_relationship!(changeset, opts)
    actor = resolve_actor(actor, opts)
    allow_nil = opts[:allow_nil?]

    case actor do
      nil when allow_nil ->
        {:atomic, %{}}

      nil ->
        {:error, "Could not relate to actor, as no actor was found (and :allow_nil? is false)."}

      actor when relationship.type == :belongs_to ->
        {:atomic,
         %{
           relationship.source_attribute => Map.get(actor, relationship.destination_attribute)
         }}

      _ ->
        {:not_atomic, "Can only use `relate_actor` atomically with a belongs_to relationship."}
    end
  end

  defp resolve_relationship!(changeset, opts) do
    relationship = Ash.Resource.Info.relationship(changeset.resource, opts[:relationship])

    if relationship.type in [:belongs_to, :has_one] do
      relationship
    else
      raise ArgumentError, """
      Cannot use `relate_actor` change with relationship of type #{inspect(relationship.type)}.

      It can only be used with a `:belongs_to` or `:has_one` relationship. If you would like to
      add the actor to a list, or something else along those lines, use a custom change
      along with `Ash.Changeset.manage_relationship`.
      """
    end
  end

  defp resolve_actor(actor, opts) do
    field = opts[:field]

    if actor == nil or field == nil do
      actor
    else
      Map.get(actor, field)
    end
  end
end
