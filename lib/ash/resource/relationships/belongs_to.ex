defmodule Ash.Resource.Relationships.BelongsTo do
  @moduledoc "Represents a belongs_to relationship on a resource"

  defstruct [
    :name,
    :destination,
    :primary_key?,
    :define_attribute?,
    :attribute_type,
    :destination_attribute,
    :private?,
    :source_attribute,
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
    validate_destination_attribute?: true,
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
          define_attribute?: boolean,
          attribute_type: term,
          writable?: boolean,
          attribute_writable?: boolean,
          destination_attribute: atom,
          private?: boolean,
          source_attribute: atom | nil,
          description: String.t()
        }

  import Ash.Resource.Relationships.SharedOptions

  alias Spark.OptionsHelpers

  @global_opts shared_options()
               |> OptionsHelpers.set_default!(:destination_attribute, :id)
               |> OptionsHelpers.append_doc!(:source_attribute, "Defaults to <name>_id")
               |> Keyword.delete(:could_be_related_at_creation?)

  @opt_schema Spark.OptionsHelpers.merge_schemas(
                [
                  primary_key?: [
                    type: :boolean,
                    links: [
                      guides: ["ash:guide:Attributes"]
                    ],
                    default: false,
                    doc:
                      "Whether the generated attribute is, or is part of, the primary key of a resource."
                  ],
                  required?: [
                    type: :boolean,
                    default: false,
                    links: [
                      guides: ["ash:guide:Attributes"]
                    ],
                    doc:
                      "Whether this relationship must always be present, e.g: must be included on creation, and never removed (it may be modified). The generated attribute will not allow nil values."
                  ],
                  attribute_writable?: [
                    type: :boolean,
                    default: false,
                    links: [
                      guides: ["ash:guide:Attributes"]
                    ],
                    doc: """
                    Whether the generated attribute will be marked as public & writable.
                    """
                  ],
                  define_attribute?: [
                    type: :boolean,
                    default: true,
                    links: [
                      guides: ["ash:guide:Attributes"]
                    ],
                    doc:
                      "If set to `false` an attribute is not created on the resource for this relationship, and one must be manually added in `attributes`, invalidating many other options."
                  ],
                  attribute_type: [
                    type: :any,
                    links: [
                      modules: [
                        "ash:module:Ash.Type"
                      ],
                      guides: ["ash:guide:Attributes"]
                    ],
                    default: :uuid,
                    doc: "The type of the generated created attribute."
                  ]
                ],
                @global_opts,
                "Relationship Options"
              )

  @doc false
  def opt_schema, do: @opt_schema
end
