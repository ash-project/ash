defmodule Ash.Resource.Relationships.HasMany do
  defstruct [
    :name,
    :type,
    :cardinality,
    :side_load,
    :path,
    :destination,
    :destination_field,
    :source_field
  ]

  @type t :: %__MODULE__{
          type: :has_many,
          cardinality: :many
        }

  @spec new(
          resource_name :: String.t(),
          name :: atom,
          related_resource :: Ash.resource(),
          opts :: Keyword.t()
        ) :: t()
  def new(resource_name, resource_type, name, related_resource, opts \\ []) do
    path = opts[:path] || resource_name <> "/:id/" <> to_string(name)

    %__MODULE__{
      name: name,
      type: :has_many,
      cardinality: :many,
      path: path,
      destination: related_resource,
      destination_field: atomize(opts[:destination_field] || "#{resource_type}_id"),
      source_field: atomize(opts[:source_field] || "id"),
      side_load: opts[:side_load]
    }
  end

  defp atomize(value) when is_atom(value), do: value
  defp atomize(value) when is_bitstring(value), do: String.to_atom(value)
end
