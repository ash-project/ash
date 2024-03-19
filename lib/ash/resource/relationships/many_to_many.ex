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
    :domain,
    :public?,
    :sort,
    :read_action,
    :description,
    :context,
    :filter,
    :has_many,
    filters: [],
    filterable?: true,
    sortable?: true,
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
          public?: boolean,
          filter: Ash.Filter.t() | nil,
          filters: list(any),
          read_action: atom,
          name: atom,
          through: Ash.Resource.t(),
          destination: Ash.Resource.t(),
          filterable?: boolean,
          sortable?: boolean,
          join_relationship: atom,
          source_attribute: atom,
          destination_attribute: atom,
          source_attribute_on_join_resource: atom,
          destination_attribute_on_join_resource: atom,
          description: String.t()
        }

  import Ash.Resource.Relationships.SharedOptions

  @global_opts shared_options()
               |> Spark.Options.Helpers.set_default!(:destination_attribute, :id)
               |> Spark.Options.Helpers.set_default!(:source_attribute, :id)

  @opt_schema Spark.Options.merge(
                [
                  source_attribute_on_join_resource: [
                    type: :atom,
                    doc:
                      "The attribute on the join resource that should line up with `source_attribute` on this resource. Defaults to `<snake_cased_last_part_of_source_module_name>_id`."
                  ],
                  destination_attribute_on_join_resource: [
                    type: :atom,
                    doc:
                      "The attribute on the join resource that should line up with `destination_attribute` on the related resource. Defaults to `<snake_cased_last_part_of_destination_module_name>_id`."
                  ],
                  through: [
                    type: Ash.OptionsHelpers.ash_resource(),
                    doc: "The resource to use as the join resource."
                  ],
                  join_relationship: [
                    type: :atom,
                    doc:
                      "The has_many relationship to the join resource. Defaults to `<relationship_name>_join_assoc`."
                  ]
                ],
                @global_opts,
                "Relationship Options"
              )

  @doc false
  def opt_schema, do: @opt_schema

  @doc false
  # sobelow_skip ["DOS.BinToAtom"]
  def transform(%{join_relationship: join_relationship, name: name} = relationship) do
    {:ok,
     %{relationship | join_relationship: join_relationship || :"#{name}_join_assoc"}
     |> Ash.Resource.Actions.Read.concat_filters()}
  end
end
