defmodule Ash.Resource.Relationships.BelongsTo do
  @moduledoc "Represents a belongs_to relationship on a resource"

  defstruct [
    :name,
    :destination,
    :primary_key?,
    :define_field?,
    :field_type,
    :destination_field,
    :private?,
    :source_field,
    :source,
    :read_action,
    :api,
    :not_found_message,
    :violation_message,
    :required?,
    :filter,
    :sort,
    :writable?,
    :context,
    :description,
    :attribute_writable?,
    validate_destination_field?: true,
    cardinality: :one,
    type: :belongs_to
  ]

  @type t :: %__MODULE__{
          type: :belongs_to,
          cardinality: :one,
          writable?: boolean,
          name: atom,
          read_action: atom,
          filter: Ash.Filter.t() | nil,
          source: Ash.Resource.t(),
          destination: Ash.Resource.t(),
          required?: boolean,
          primary_key?: boolean,
          define_field?: boolean,
          field_type: term,
          writable?: boolean,
          attribute_writable?: boolean,
          destination_field: atom,
          private?: boolean,
          source_field: atom | nil,
          description: String.t()
        }

  import Ash.Resource.Relationships.SharedOptions

  alias Ash.OptionsHelpers

  @global_opts shared_options()
               |> OptionsHelpers.set_default!(:destination_field, :id)
               |> OptionsHelpers.append_doc!(:source_field, "Defaults to <name>_id")
               |> Keyword.delete(:could_be_related_at_creation?)

  @opt_schema Ash.OptionsHelpers.merge_schemas(
                [
                  primary_key?: [
                    type: :boolean,
                    default: false,
                    doc: "Whether this field is, or is part of, the primary key of a resource."
                  ],
                  required?: [
                    type: :boolean,
                    default: false,
                    doc:
                      "Whether this relationship must always be present, e.g: must be included on creation, and never removed (it can still be changed)"
                  ],
                  attribute_writable?: [
                    type: :boolean,
                    default: false,
                    doc: """
                    Whether this relationship's generated attribute will be marked as writable.

                    Has no effect when combined with `define_field?: false`.
                    """
                  ],
                  define_field?: [
                    type: :boolean,
                    default: true,
                    doc:
                      "If set to `false` a field is not created on the resource for this relationship, and one must be manually added in `attributes`."
                  ],
                  field_type: [
                    type: :any,
                    default: :uuid,
                    doc: "The field type of the automatically created field."
                  ]
                ],
                @global_opts,
                "Relationship Options"
              )

  @doc false
  def opt_schema, do: @opt_schema
end
