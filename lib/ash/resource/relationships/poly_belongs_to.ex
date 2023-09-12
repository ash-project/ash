defmodule Ash.Resource.Relationships.PolyBelongsTo do
  @moduledoc "Represents a belongs_to relationship on a resource"

  defstruct [
    :name,
    :types,
    :define_attribute?,
    :define_type?,
    :attribute_type,
    :destination_attribute,
    :type_attribute,
    :private?,
    :source_attribute,
    :source,
    :read_action,
    :api,
    :not_found_message,
    :violation_message,
    :allow_nil?,
    :writable?,
    :context,
    :description,
    :attribute_writable?,
    validate_destination_attribute?: true,
    cardinality: :one,
    type: :poly_belongs_to
    # :primary_key?,
    # filterable?: true,
    # :filter,
    # :sort,
  ]

  @type t :: %__MODULE__{
          type: :belongs_to,
          cardinality: :one,
          writable?: boolean,
          name: atom,
          types: map,
          read_action: atom,
          source: Ash.Resource.t(),
          allow_nil?: boolean,
          define_attribute?: boolean,
          define_type?: boolean,
          attribute_type: term,
          writable?: boolean,
          attribute_writable?: boolean,
          destination_attribute: atom,
          type_attribute: atom,
          private?: boolean,
          source_attribute: atom | nil,
          description: String.t()
          # primary_key?: boolean,
          # filterable?: boolean,
          # filter: Ash.Filter.t() | nil,
          # sort: ?
        }

  import Ash.Resource.Relationships.SharedOptions

  alias Spark.OptionsHelpers

  @global_opts shared_options()
               |> OptionsHelpers.set_default!(:destination_attribute, :id)
               |> OptionsHelpers.append_doc!(:source_attribute, "Defaults to <name>_id")
               |> Keyword.drop([
                 :destination,
                 :primary_key?,
                 :filterable?,
                 :filter,
                 :sort,
                 :could_be_related_at_creation?
               ])

  @opt_schema Spark.OptionsHelpers.merge_schemas(
                [
                  types: [
                    # type: {:map, Ash.OptionsHelpers.ash_resource(), :string},
                    type: {:map, :atom, :string},
                    doc:
                      "Map containing resource types that this relationship may point to, and a stable type name used to record them"
                  ],
                  # primary_key?: [
                  #  type: :boolean,
                  #  default: false,
                  #  doc:
                  #    "Whether the generated attribute is, or is part of, the primary key of a resource."
                  # ],
                  allow_nil?: [
                    type: :boolean,
                    default: true,
                    doc:
                      "Whether this relationship must always be present, e.g: must be included on creation, and never removed (it may be modified). The generated attribute will not allow nil values."
                  ],
                  attribute_writable?: [
                    type: :boolean,
                    default: false,
                    doc: "Whether the generated attribute will be marked as public & writable."
                  ],
                  define_attribute?: [
                    type: :boolean,
                    default: true,
                    doc:
                      "If set to `false` an attribute is not created on the resource for this relationship, and one must be manually added in `attributes`, invalidating many other options."
                  ],
                  define_type?: [
                    type: :boolean,
                    default: true,
                    doc:
                      "If set to `false` an attribute is not created on the resource for this relationship, and one must be manually added in `attributes`, invalidating many other options."
                  ],
                  type_attribute: [
                    type: :atom,
                    doc:
                      "Field on this resource that stores the type name of the related resource, defaults to <name>_type"
                  ],
                  attribute_type: [
                    type: :any,
                    default: Application.compile_env(:ash, :default_belongs_to_type, :uuid),
                    doc: "The type of the generated created attribute. See `Ash.Type` for more."
                  ]
                ],
                @global_opts,
                "Relationship Options"
              )

  @doc false
  def opt_schema, do: @opt_schema
end
