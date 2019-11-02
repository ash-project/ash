defmodule Ash.Resource.Relationships.BelongsTo do
  defstruct [
    :name,
    :cardinality,
    :type,
    :path,
    :destination,
    :primary_key?,
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
      primary_key?: Keyword.get(opts, :primary_key, false),
      destination: related_resource,
      destination_field: atomize(opts[:destination_field] || "id"),
      source_field: atomize(opts[:source_field] || "#{name}_id"),
      side_load: opts[:side_load]
    }
  end

  defp atomize(value) when is_atom(value), do: value
  defp atomize(value) when is_bitstring(value), do: String.to_atom(value)
end
