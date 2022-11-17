defmodule Ash.Resource.Relationships.SharedOptions do
  @moduledoc false

  @shared_options [
    name: [
      type: :atom,
      doc: "The name of the relationship",
      links: []
    ],
    destination: [
      type: Ash.OptionsHelpers.ash_resource(),
      doc: "The destination resource",
      links: []
    ],
    description: [
      type: :string,
      doc: "An optional description for the relationship",
      links: []
    ],
    destination_attribute: [
      type: :atom,
      links: [
        dsls: [
          "ash:dsl:resource/attributes/attribute"
        ]
      ],
      doc:
        "The attribute on the related resource that should match the `source_attribute` configured on this resource."
    ],
    validate_destination_attribute?: [
      type: :boolean,
      default: true,
      doc:
        "Whether or not to validate that the destination field exists on the destination resource",
      links: []
    ],
    source_attribute: [
      type: :atom,
      links: [
        dsls: [
          "ash:dsl:resource/attributes/attribute"
        ]
      ],
      doc:
        "The field on this resource that should match the `destination_attribute` on the related resource."
    ],
    relationship_context: [
      type: :any,
      as: :context,
      links: [],
      doc: """
      Context to be set on any queries or changesets generated for managing or querying this relationship.
      """
    ],
    private?: [
      type: :boolean,
      default: false,
      links: [
        guides: ["ash:guide:Security"]
      ],
      doc:
        "Whether or not the relationship will appear in any interfaces created off of this resource, e.g AshJsonApi and AshGraphql"
    ],
    not_found_message: [
      type: :string,
      doc: """
      A message to show if there is a conflict with this relationship in the database on update or create, or when managing relationships.
      """,
      links: [
        guides: ["ash:guide:Managing Relationships"]
      ]
    ],
    writable?: [
      type: :boolean,
      default: true,
      doc: """
      Whether or not the relationship may be managed.
      """,
      links: []
    ],
    read_action: [
      type: :atom,
      doc: """
      The read action on the destination resource to use when loading data and filtering.
      """,
      links: []
    ],
    api: [
      type: :atom,
      doc: """
      The API module to use when working with the related entity.
      """,
      links: []
    ],
    filter: [
      type: :any,
      doc: """
      A filter to be applied when reading the relationship.
      """,
      links: []
    ],
    filterable?: [
      type: :boolean,
      default: true,
      doc: "If set to `false`, the relationship will not be usable in filters."
    ],
    sort: [
      type: :any,
      doc: """
      A sort statement to be applied when loading the relationship.
      """,
      links: []
    ],
    could_be_related_at_creation?: [
      type: :boolean,
      default: false,
      links: [],
      doc: """
      Whether or not related values may exist for this relationship at creation.
      """
    ],
    violation_message: [
      type: :string,
      links: [],
      doc: """
      A message to show if there is a conflict with this relationship in the database on destroy.
      For example, if a record is deleted while related records still exist (and aren't configured to cascade deletes)
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
       links: [],
       doc: """
       If true, all existing entities are considered related, i.e this relationship is not based on any fields, and `source_attribute` and
       `destination_attribute` are ignored.

       This can be very useful when combined with multitenancy. Specifically, if you have a tenant resource like `Organization`,
       you can use `no_attributes?` to do things like `has_many :employees, Employee, no_attributes?: true`, which lets you avoid having an
       unnecessary `organization_id` field on `Employee`. The same works in reverse: `has_one :organization, Organization, no_attributes?: true`
       allows relating the employee to their organization.

       Some important caveats here:

       1. You can still manage relationships from one to the other, but "relate" and "unrelate"
       will have no effect, because there are no fields to change.

       2. Loading the relationship on a list of resources will not behave as expected in all circumstances involving multitenancy. For example,
          if you get a list of `Organization` and then try to load `employees`, you would need to set a single tenant on the load query, meaning
          you'll get all organizations back with the set of employees from one tenant. This could eventually be solved, but for now it is considered an
          edge case.
       """
     ]}
  end

  def manual do
    {:manual,
     type:
       {:spark_function_behaviour, Ash.Resource.ManualRelationship,
        {Ash.Resource.ManualRelationship.Function, 2}},
     links: [],
     doc: """
     A module that implements `Ash.Resource.ManualRelationship`. Also accepts a 2 argument function that takes the source records and the context.
     ```
     """}
  end
end
