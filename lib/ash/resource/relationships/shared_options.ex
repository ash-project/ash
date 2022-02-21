defmodule Ash.Resource.Relationships.SharedOptions do
  @moduledoc false

  @shared_options [
    name: [
      type: :atom,
      doc: "The name of the relationship"
    ],
    destination: [
      type: :ash_resource,
      doc: "The destination resource"
    ],
    destination_field: [
      type: :atom,
      doc:
        "The field on the related resource that should match the `source_field` on this resource."
    ],
    validate_destination_field?: [
      type: :boolean,
      default: true,
      doc:
        "Whether or not to validate that the destination field exists on the destination resource"
    ],
    source_field: [
      type: :atom,
      doc:
        "The field on this resource that should match the `destination_field` on the related resource."
    ],
    writable?: [
      type: :boolean,
      doc: "Whether or not the relationship may be edited.",
      default: true
    ],
    description: [
      type: :string,
      doc: "An optional description for the relationship"
    ],
    relationship_context: [
      type: :any,
      as: :context,
      doc: """
      Context to be set on any queries or changesets generated for this relationship.

      This is used by ash_postgres for polymorphic resources.
      """
    ],
    private?: [
      type: :boolean,
      default: false,
      doc:
        "Whether or not the relationship will appear in any interfaces created off of this resource, e.g AshJsonApi and AshGraphql"
    ],
    not_found_message: [
      type: :string,
      doc: """
      A message to show if there is a conflict with this relationship in the database on update or create.

      For example, if a value is added that has no match in the destination (very hard to do with the way Ash relationship changes work).
      """
    ],
    read_action: [
      type: :atom,
      doc: """
      The read action on the destination resource to use when loading data.

      Keep in mind, any filters that exist on the destination action are not honored when filtering on this
      relationship. The only time the read action comes into play is when loading the actual relationship, which happens when they are loaded
      explicitly and when the relationship is managed.
      """
    ],
    filter: [
      type: :any,
      doc: """
      A filter to be applied when reading the relationship.
      """
    ],
    sort: [
      type: :any,
      doc: """
      A sort statement to be applied when reading the relationship.
      """
    ],
    violation_message: [
      type: :string,
      doc: """
      A message to show if there is a conflict with this relationship in the database on destroy.
      For example, if a record is deleted while related records still exist (and aren't configured to cascade deletes)
      """
    ]
  ]

  def shared_options do
    @shared_options
  end

  def manual do
    {:manual,
     type: {:ash_behaviour, Ash.Resource.ManualRelationship},
     doc: """
     Allows for relationships that are fetched manually. WARNING: EXPERIMENTAL

     Manual relationships do not support filters or aggregates at the moment. In the future, what we may do is
     allow the data layer to be configured with a hook that expresses how to implement this manual relationship
     at the data layer level, like providing a custom ecto join for ash_postgres. This is the simple groundwork
     for that.

     ```elixir
     # in the resource
     relationships do
       has_many :somethings, MyApp.Something do
         manual {MyApp.FetchSomethings, [opt1: :value]}
         # or if there are no opts
         # manual MyApp.FetchSomethings
       end
     end

     # the implementation
     defmodule MyApp.FetchSomethings do
       use Ash.Resource.ManualRelationship

       def load(records, _opts, %{relationship: relationship}) do
         # Return a map of primary keys of the records to the related records.
         # This example is likely suboptimal because it does a separate fetch for
         # each record, whereas you likely want to try to fetch them all at once,
         # and then create the mapping from pkey values to related records
         # For example:

         # get the primary key
         primary_key = Ash.Resource.Info.primary_key(relationship.source)
         # e.g [:id]

         # key the records by primary key and the related records with that primary key
         {:ok,
           Map.new(records, fn record ->
             # the key is the pkey values, e.g `%{id: 1}`
             # the value is the related records for that record
             {Map.take(record, primary_key), get_related_records(record)}
           end)}
       end
     end
     ```
     """}
  end
end
