defmodule Ash.Resource.Relationships.BelongsTo do
  @moduledoc "Represents a belongs_to relationship on a resource"

  defstruct [
    :name,
    :destination,
    :primary_key?,
    :define_field?,
    :field_type,
    :destination_field,
    :source_field,
    :source,
    :required?,
    :writable?,
    :description,
    cardinality: :one,
    type: :belongs_to
  ]

  @type t :: %__MODULE__{
          type: :belongs_to,
          cardinality: :one,
          writable?: boolean,
          name: atom,
          source: Ash.resource(),
          destination: Ash.resource(),
          required?: boolean,
          primary_key?: boolean,
          define_field?: boolean,
          field_type: Ash.Type.t(),
          destination_field: atom,
          source_field: atom | nil,
          description: String.t()
        }

  import Ash.Resource.Relationships.SharedOptions, only: [shared_options: 0]

  alias Ash.OptionsHelpers

  @global_opts shared_options()
               |> OptionsHelpers.set_default!(:destination_field, :id)
               |> OptionsHelpers.append_doc!(:source_field, "Defaults to <name>_id")

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
                  define_field?: [
                    type: :boolean,
                    default: true,
                    doc:
                      "If set to `false` a field is not created on the resource for this relationship, and one must be manually added in `attributes`."
                  ],
                  field_type: [
                    type: {:custom, OptionsHelpers, :ash_type, []},
                    default: :uuid,
                    doc: "The field type of the automatically created field."
                  ],
                  description: [
                    type: :string,
                    doc: "An optional description for the belongs_to relationship"
                  ]
                ],
                @global_opts,
                "Relationship Options"
              )

  @doc false
  def opt_schema, do: @opt_schema
end
