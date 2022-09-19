defmodule Ash.Resource.Change.RelateActor do
  @moduledoc false
  use Ash.Resource.Change
  alias Ash.Changeset
  alias Ash.Error.Changes.InvalidRelationship

  @opt_schema [
    relationship: [
      doc: "The relationship to set the actor to.",
      required: true,
      type: :atom
    ],
    allow_nil?: [
      doc: "Wether or not to allow the actor to be nil, in which case nothing will happen.",
      type: :boolean,
      default: false
    ]
  ]

  def opt_schema, do: @opt_schema

  def init(opts) do
    case Spark.OptionsHelpers.validate(opts, @opt_schema) do
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
