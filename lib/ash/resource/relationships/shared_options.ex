defmodule Ash.Resource.Relationships.SharedOptions do
  @moduledoc false

  @shared_options [
    destination_field: [
      type: :atom,
      doc:
        "The field on the related resource that should match the `source_field` on this resource. Default: [resource.name]_id"
    ],
    source_field: [
      type: :atom,
      doc:
        "The field on this resource that should match the `destination_field` on the related resource."
    ],
    reverse_relationship: [
      type: :atom,
      doc:
        "A requirement for side loading data. Must be the name of an inverse relationship on the destination resource."
    ]
  ]

  def shared_options do
    @shared_options
  end

  def make_required!(options, field) do
    Keyword.update!(options, field, &Keyword.put(&1, :required, true))
  end

  def set_default!(options, field, value) do
    Keyword.update!(options, field, &Keyword.put(&1, :default, value))
  end
end
