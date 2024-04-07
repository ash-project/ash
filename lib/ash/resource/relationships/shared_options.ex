defmodule Ash.Resource.Relationships.SharedOptions do
  @moduledoc false

  @shared_options [
    name: [
      type: :atom,
      doc: "The name of the relationship"
    ],
    destination: [
      type: Ash.OptionsHelpers.ash_resource(),
      doc: "The destination resource"
    ],
    description: [
      type: :string,
      doc: "An optional description for the relationship"
    ],
    destination_attribute: [
      type: :atom,
      doc:
        "The attribute on the related resource that should match the `source_attribute` configured on this resource."
    ],
    validate_destination_attribute?: [
      type: :boolean,
      default: true,
      doc:
        "Whether or not to validate that the destination field exists on the destination resource"
    ],
    source_attribute: [
      type: :atom,
      doc:
        "The field on this resource that should match the `destination_attribute` on the related resource."
    ],
    relationship_context: [
      type: :any,
      as: :context,
      doc: """
      Context to be set on any queries or changesets generated for managing or querying this relationship.
      """
    ],
    public?: [
      type: :boolean,
      default: false,
      doc: """
      Whether or not the relationship will appear in public interfaces
      """
    ],
    not_found_message: [
      type: :string,
      doc: """
      A message to show if there is a conflict with this relationship in the database on update or create, or when managing relationships.
      """
    ],
    writable?: [
      type: :boolean,
      default: true,
      doc: """
      Whether or not the relationship may be managed.
      """
    ],
    read_action: [
      type: :atom,
      doc: """
      The read action on the destination resource to use when loading data and filtering.
      """
    ],
    domain: [
      type: :atom,
      doc: """
      The domain module to use when working with the related entity.
      """
    ],
    filterable?: [
      type: :boolean,
      default: true,
      doc: "If set to `false`, the relationship will not be usable in filters."
    ],
    sortable?: [
      type: :boolean,
      default: true,
      doc: "If set to `false`, the relationship will not be usable in filters."
    ],
    sort: [
      type: :any,
      doc: """
      A sort statement to be applied when loading the relationship.
      """
    ],
    could_be_related_at_creation?: [
      type: :boolean,
      default: false,
      doc: """
      Whether or not related values may exist for this relationship at creation.
      """
    ],
    violation_message: [
      type: :string,
      doc: """
      A message to show if there is a conflict with this relationship in the database on destroy.
      """
    ]
  ]

  def shared_options do
    @shared_options
  end

  def no_attributes do
    {:no_attributes?,
     [
       type: :boolean,
       doc: """
       All existing entities are considered related, i.e this relationship is not based on any fields, and `source_attribute` and `destination_attribute` are ignored. See the See the [relationships guide](/documentation/topics/resources/relationships.md) for more.
       """
     ]}
  end

  def manual do
    {:manual,
     type:
       {:spark_function_behaviour, Ash.Resource.ManualRelationship,
        {Ash.Resource.ManualRelationship.Function, 2}},
     doc: """
     A module that implements `Ash.Resource.ManualRelationship`. Also accepts a 2 argument function that takes the source records and the context.
     """}
  end
end
