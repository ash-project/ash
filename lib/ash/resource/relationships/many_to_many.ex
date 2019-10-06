defmodule Ash.Resource.Relationships.ManyToMany do
  defstruct [
    :name,
    :type,
    :through,
    :cardinality,
    :expose?,
    :side_load,
    :path,
    :destination,
    :source_field,
    :destination_field,
    :source_field_on_join_table,
    :destination_field_on_join_table
  ]

  def new(resource_name, name, related_resource, opts \\ []) do
    path = opts[:path] || resource_name <> "/:id/" <> to_string(name)

    through = through!(opts)

    source_field_on_join_table =
      opts[:source_field_on_join_table] || String.to_atom(resource_name <> "_id")

    destination_field_on_join_table =
      opts[:destination_field_on_join_table] ||
        String.to_atom(Ash.name(related_resource) <> "_id")

    %__MODULE__{
      name: name,
      type: :many_to_many,
      cardinality: :many,
      expose?: opts[:expose?] || false,
      path: path,
      through: through,
      side_load: opts[:side_load],
      destination: related_resource,
      source_field: opts[:source_field] || :id,
      destination_field: opts[:destination_field] || :id,
      source_field_on_join_table: source_field_on_join_table,
      destination_field_on_join_table: destination_field_on_join_table
    }
  end

  defp through!(opts) do
    case opts[:through] do
      through when is_bitstring(through) ->
        through

      _ ->
        raise "`:through` option must be a string representing a join table"
    end
  end
end
