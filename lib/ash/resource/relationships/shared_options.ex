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
    ]
  ]

  def shared_options do
    @shared_options
  end
end
