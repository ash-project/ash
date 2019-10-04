defmodule Ash.Resource.Relationships.BelongsTo do
  defstruct [:name, :type, :destination, :destination_field, :source_field]

  def new(name, related_resource, opts \\ []) do
    %__MODULE__{
      name: name,
      type: :belongs_to,
      destination: related_resource,
      destination_field: opts[:destination_field] || "id",
      source_field: opts[:source_field] || "#{name}_id"
    }
  end
end
