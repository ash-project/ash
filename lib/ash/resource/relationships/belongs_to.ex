defmodule Ash.Resource.Relationships.BelongsTo do
  defstruct [:name, :expose?, :type, :route, :destination, :destination_field, :source_field]

  def new(resource_name, name, related_resource, opts \\ []) do
    %__MODULE__{
      name: name,
      type: :belongs_to,
      expose?: opts[:expose?] || false,
      route: resource_name <> "/:id/" <> to_string(name),
      destination: related_resource,
      destination_field: opts[:destination_field] || "id",
      source_field: opts[:source_field] || "#{name}_id"
    }
  end
end
