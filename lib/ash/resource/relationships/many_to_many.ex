defmodule Ash.Resource.Relationships.ManyToMany do
  @moduledoc false
  defstruct [
    :name,
    :source,
    :through,
    :destination,
    :source_field,
    :destination_field,
    :source_field_on_join_table,
    :destination_field_on_join_table,
    :reverse_relationship,
    cardinality: :many,
    type: :many_to_many
  ]

  @type t :: %__MODULE__{
          type: :many_to_many,
          cardinality: :many,
          source: Ash.resource(),
          name: atom,
          through: Ash.resource(),
          destination: Ash.resource(),
          source_field: atom,
          destination_field: atom,
          source_field_on_join_table: atom,
          destination_field_on_join_table: atom,
          reverse_relationship: atom
        }

  import Ash.Resource.Relationships.SharedOptions
  alias Ash.OptionsHelpers

  @global_opts shared_options()
               |> OptionsHelpers.set_default!(:destination_field, :id)
               |> OptionsHelpers.set_default!(:source_field, :id)

  @opt_schema Ash.OptionsHelpers.merge_schemas(
                [
                  source_field_on_join_table: [
                    type: :atom,
                    required: true,
                    doc:
                      "The field on the join table that should line up with `source_field` on this resource. Default: [resource_name]_id"
                  ],
                  destination_field_on_join_table: [
                    type: :atom,
                    required: true,
                    doc:
                      "The field on the join table that should line up with `destination_field` on the related resource. Default: [relationshihp_name]_id"
                  ],
                  through: [
                    type: :atom,
                    required: true,
                    doc: "The resource to use as the join resource."
                  ]
                ],
                @global_opts,
                "Relationship Options"
              )

  @doc false
  def opt_schema, do: @opt_schema
end
