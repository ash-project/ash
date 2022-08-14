defmodule Ash.Resource.Relationships.HasOne do
  @moduledoc "Represents a has_one relationship on a resource"

  defstruct [
    :name,
    :source,
    :destination,
    :destination_field,
    :private?,
    :source_field,
    :allow_orphans?,
    :context,
    :description,
    :filter,
    :api,
    :sort,
    :read_action,
    :not_found_message,
    :violation_message,
    :manual,
    :writable?,
    no_fields?: false,
    could_be_related_at_creation?: false,
    validate_destination_field?: true,
    cardinality: :one,
    type: :has_one,
    required?: false
  ]

  @type t :: %__MODULE__{
          type: :has_one,
          cardinality: :one,
          source: Ash.Resource.t(),
          name: atom,
          read_action: atom,
          no_fields?: boolean,
          writable?: boolean,
          type: Ash.Type.t(),
          filter: Ash.Filter.t() | nil,
          destination: Ash.Resource.t(),
          destination_field: atom,
          private?: boolean,
          source_field: atom,
          allow_orphans?: boolean,
          description: String.t(),
          manual: atom | {atom, Keyword.t()} | nil
        }

  import Ash.Resource.Relationships.SharedOptions
  alias Spark.OptionsHelpers

  @global_opts shared_options()
               |> OptionsHelpers.set_default!(:source_field, :id)

  @opt_schema Spark.OptionsHelpers.merge_schemas(
                [manual(), no_fields()] ++
                  [
                    required?: [
                      type: :boolean,
                      links: [],
                      doc: """
                      Marks the relationship as required. Has no effect on validations, but can inform extensions that there will always be a related entity.
                      """
                    ]
                  ],
                @global_opts,
                "Relationship Options"
              )

  @doc false
  def opt_schema, do: @opt_schema
end
