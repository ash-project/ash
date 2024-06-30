defmodule Ash.Resource.Relationships.HasOne do
  @moduledoc "Represents a has_one relationship on a resource"

  defstruct [
    :name,
    :source,
    :destination,
    :destination_attribute,
    :public?,
    :source_attribute,
    :allow_orphans?,
    :context,
    :description,
    :filter,
    :domain,
    :sort,
    :read_action,
    :not_found_message,
    :violation_message,
    :manual,
    :writable?,
    filters: [],
    from_many?: false,
    no_attributes?: false,
    could_be_related_at_creation?: false,
    validate_destination_attribute?: true,
    cardinality: :one,
    type: :has_one,
    allow_nil?: false,
    filterable?: true,
    sortable?: true
  ]

  @type t :: %__MODULE__{
          type: :has_one,
          cardinality: :one,
          source: Ash.Resource.t(),
          name: atom,
          filterable?: boolean,
          sortable?: boolean,
          from_many?: boolean,
          read_action: atom,
          no_attributes?: boolean,
          writable?: boolean,
          type: Ash.Type.t(),
          filter: Ash.Filter.t() | nil,
          filters: [any],
          destination: Ash.Resource.t(),
          destination_attribute: atom,
          public?: boolean,
          source_attribute: atom,
          allow_orphans?: boolean,
          description: String.t(),
          manual: atom | {atom, Keyword.t()} | nil
        }

  import Ash.Resource.Relationships.SharedOptions

  @global_opts shared_options()
               |> Spark.Options.Helpers.set_default!(:source_attribute, :id)

  @opt_schema Spark.Options.merge(
                [manual(), no_attributes()] ++
                  [
                    allow_nil?: [
                      type: :boolean,
                      default: true,
                      doc: """
                      Marks the relationship as required. Has no effect on validations, but can inform extensions that there will always be a related entity.
                      """
                    ],
                    from_many?: [
                      type: :boolean,
                      default: false,
                      doc: """
                      Signal that this relationship is actually a `has_many` where the first record is given via the `sort`. This will allow data layers to properly deduplicate when necessary.
                      """
                    ]
                  ],
                @global_opts,
                "Relationship Options"
              )

  @doc false
  def opt_schema, do: @opt_schema

  def transform(relationship) do
    {:ok,
     relationship
     |> Ash.Resource.Actions.Read.concat_filters()
     |> Map.put(:from_many?, relationship.from_many? || not is_nil(relationship.sort))}
  end
end
