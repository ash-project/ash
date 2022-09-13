defmodule Ash.Resource.Relationships.ManyToMany do
  @moduledoc "Represents a many_to_many relationship on a resource"
  defstruct [
    :name,
    :source,
    :through,
    :destination,
    :source_attribute,
    :destination_attribute,
    :source_attribute_on_join_resource,
    :destination_attribute_on_join_resource,
    :join_relationship,
    :not_found_message,
    :violation_message,
    :api,
    :private?,
    :sort,
    :read_action,
    :description,
    :context,
    :filter,
    :has_many,
    filterable?: true,
    could_be_related_at_creation?: false,
    validate_destination_attribute?: true,
    cardinality: :many,
    type: :many_to_many
  ]

  @type t :: %__MODULE__{
          type: :many_to_many,
          cardinality: :many,
          source: Ash.Resource.t(),
          has_many: boolean,
          private?: boolean,
          filter: Ash.Filter.t() | nil,
          read_action: atom,
          name: atom,
          through: Ash.Resource.t(),
          destination: Ash.Resource.t(),
          filterable?: boolean,
          join_relationship: atom,
          source_attribute: atom,
          destination_attribute: atom,
          source_attribute_on_join_resource: atom,
          destination_attribute_on_join_resource: atom,
          description: String.t()
        }

  import Ash.Resource.Relationships.SharedOptions
  alias Spark.OptionsHelpers

  @global_opts shared_options()
               |> OptionsHelpers.set_default!(:destination_attribute, :id)
               |> OptionsHelpers.set_default!(:source_attribute, :id)

  @opt_schema Spark.OptionsHelpers.merge_schemas(
                [
                  source_attribute_on_join_resource: [
                    type: :atom,
                    required: true,
                    links: [
                      dsls: [
                        "ash:dsl:resource/attributes/attribute"
                      ]
                    ],
                    doc:
                      "The attribute on the join resource that should line up with `source_attribute` on this resource."
                  ],
                  destination_attribute_on_join_resource: [
                    type: :atom,
                    required: true,
                    links: [
                      dsls: [
                        "ash:dsl:resource/attributes/attribute"
                      ]
                    ],
                    doc:
                      "The attribute on the join resource that should line up with `destination_attribute` on the related resource."
                  ],
                  through: [
                    type: Ash.OptionsHelpers.ash_resource(),
                    required: true,
                    links: [],
                    doc: "The resource to use as the join resource."
                  ],
                  join_relationship: [
                    type: :atom,
                    links: [],
                    doc:
                      "The has_many relationship to the join resource. Defaults to <relationship_name>_join_assoc"
                  ]
                ],
                @global_opts,
                "Relationship Options"
              )

  @doc false
  def opt_schema, do: @opt_schema

  # sobelow_skip ["DOS.StringToAtom"]
  def transform(%{join_relationship: nil, name: name} = relationship) do
    {:ok, %{relationship | join_relationship: String.to_atom("#{name}_join_assoc")}}
  end

  def transform(relationship), do: {:ok, relationship}
end
