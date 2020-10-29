defmodule Ash.Resource.Relationships.SharedOptions do
  @moduledoc false

  @shared_options [
    name: [
      type: :atom,
      doc: "The name of the relationship"
    ],
    destination: [
      type: :atom,
      doc: "The destination resource"
    ],
    destination_field: [
      type: :atom,
      doc:
        "The field on the related resource that should match the `source_field` on this resource."
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
    private?: [
      type: :boolean,
      default: false,
      doc: "Whether or not the value can be accessed by extensions"
    ]
  ]

  def shared_options do
    @shared_options
  end
end
