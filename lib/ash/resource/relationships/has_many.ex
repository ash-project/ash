defmodule Ash.Resource.Relationships.HasMany do
  defstruct [
    :name,
    :type,
    :cardinality,
    :side_load,
    :expose?,
    :path,
    :destination,
    :destination_field,
    :source_field
  ]

  def new(resource_name, name, related_resource, opts \\ []) do
    path = opts[:path] || resource_name <> "/:id/" <> to_string(name)

    %__MODULE__{
      name: name,
      type: :has_many,
      cardinality: :many,
      expose?: opts[:expose?] || false,
      path: path,
      destination: related_resource,
      destination_field: opts[:destination_field] || "#{resource_name}_id",
      source_field: opts[:source_field] || "id",
      side_load: opts[:side_load]
    }
  end
end
