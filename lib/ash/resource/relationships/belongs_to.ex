defmodule Ash.Resource.Relationships.BelongsTo do
  defstruct [
    :name,
    :expose?,
    :cardinality,
    :type,
    :path,
    :destination,
    :side_load,
    :destination_field,
    :source_field
  ]

  def new(resource_name, name, related_resource, opts \\ []) do
    path = opts[:path] || resource_name <> "/:id/" <> to_string(name)

    %__MODULE__{
      name: name,
      type: :belongs_to,
      cardinality: :one,
      expose?: opts[:expose?] || false,
      path: path,
      destination: related_resource,
      destination_field: opts[:destination_field] || "id",
      source_field: opts[:source_field] || "#{name}_id",
      side_load: opts[:side_load]
    }
  end
end
