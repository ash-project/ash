# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Resource.Change.RelateActor do
  @moduledoc false
  use Ash.Resource.Change
  import Ash.Gettext
  alias Ash.Changeset
  alias Ash.Error.Changes.InvalidRelationship

  @opt_schema [
    relationship: [
      doc: "The relationship to set the actor to.",
      required: true,
      type: :atom,
      hide: true
    ],
    allow_nil?: [
      doc: "Whether or not to allow the actor to be nil, in which case nothing will happen.",
      type: :boolean,
      default: false
    ],
    field: [
      doc: "The field of the actor to set the relationship to",
      type: :atom
    ]
  ]

  def opt_schema, do: @opt_schema

  opt_schema = @opt_schema

  defmodule Opts do
    @moduledoc false
    use Spark.Options.Validator, schema: opt_schema
  end

  @impl true
  def init(opts) do
    case Opts.validate(opts) do
      {:ok, opts} ->
        {:ok, Opts.to_options(opts)}

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
            message:
              error_message(
                "could not relate to actor, as no actor was found (and :allow_nil? is false)"
              )
          )
        )

      actor when relationship.type == :belongs_to ->
        Changeset.force_change_attribute(
          changeset,
          relationship.source_attribute,
          resolve_fk_value(actor, relationship)
        )
        |> Changeset.after_action(fn _changeset, record ->
          {:ok, Map.put(record, relationship.name, actor)}
        end)

      actor ->
        Changeset.manage_relationship(
          changeset,
          relationship.name,
          actor,
          type: :append_and_remove,
          authorize?: false
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
           relationship.source_attribute => resolve_fk_value(actor, relationship)
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

  # When `field:` is used, `resolve_actor/2` may return a scalar value
  # (e.g. a UUID string) rather than a map/struct. In that case, use
  # the value directly as the FK instead of extracting destination_attribute.
  defp resolve_fk_value(actor, relationship) when is_map(actor) do
    Map.get(actor, relationship.destination_attribute)
  end

  defp resolve_fk_value(actor, _relationship), do: actor

  defp resolve_actor(actor, opts) do
    field = opts[:field]

    if actor == nil or field == nil do
      actor
    else
      Map.get(actor, field)
    end
  end
end
