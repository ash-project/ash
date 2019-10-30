defmodule Ash.Resource.Relationships.BelongsTo do
  defstruct [
    :name,
    :cardinality,
    :type,
    :path,
    :destination,
    :side_load,
    :destination_field,
    :source_field
  ]

  @type t :: %__MODULE__{
          type: :belongs_to,
          cardinality: :one
        }

  @spec new(
          resource_name :: String.t(),
          name :: atom,
          related_resource :: Ash.resource(),
          opts :: Keyword.t()
        ) :: t()
  def new(resource_name, name, related_resource, opts \\ []) do
    path = opts[:path] || resource_name <> "/:id/" <> to_string(name)

    %__MODULE__{
      name: name,
      type: :belongs_to,
      cardinality: :one,
      path: path,
      destination: related_resource,
      destination_field: opts[:destination_field] || "id",
      source_field: opts[:source_field] || "#{name}_id",
      side_load: opts[:side_load]
    }
  end
end
