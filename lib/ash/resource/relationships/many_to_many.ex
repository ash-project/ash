defmodule Ash.Resource.Relationships.ManyToMany do
  defstruct [
    :name,
    :type,
    :through,
    :cardinality,
    :side_load,
    :path,
    :destination,
    :source_field,
    :destination_field,
    :source_field_on_join_table,
    :destination_field_on_join_table
  ]

  @type t :: %__MODULE__{
          type: :many_to_many,
          cardinality: :many
        }

  @spec new(
          resource_name :: String.t(),
          name :: atom,
          related_resource :: Ash.resource(),
          opts :: Keyword.t()
        ) :: t()
  def new(resource_name, name, related_resource, opts \\ []) do
    path = opts[:path] || resource_name <> "/:id/" <> to_string(name)

    source_field_on_join_table =
      atomize(opts[:source_field_on_join_table] || String.to_atom(resource_name <> "_id"))

    destination_field_on_join_table =
      opts[:destination_field_on_join_table] ||
        raise """
          Must set `:destination_field_on_join_table` for #{resource_name}.#{name} as it cannot currently be derived.
        """

    source_field = atomize(opts[:source_field] || :id)
    destination_field = atomize(opts[:destination_field] || :id)

    through =
      through!(
        opts,
        source_field_on_join_table,
        destination_field_on_join_table
      )

    %__MODULE__{
      name: name,
      type: :many_to_many,
      cardinality: :many,
      path: path,
      through: through,
      side_load: opts[:side_load],
      destination: related_resource,
      source_field: source_field,
      destination_field: destination_field,
      source_field_on_join_table: source_field_on_join_table,
      destination_field_on_join_table: destination_field_on_join_table
    }
  end

  defp atomize(value) when is_atom(value), do: value
  defp atomize(value) when is_bitstring(value), do: String.to_atom(value)

  defp through!(opts, _source_field_on_join_table, _destination_field_on_join_table) do
    case opts[:through] do
      through when is_atom(through) ->
        through

      # TODO: do this check at runtime. When done at compilation, it forces the modules
      # to be compiled, which causes warnings in ecto.
      # case Ash.primary_key(through) do
      #   [^source_field_on_join_table, ^destination_field_on_join_table] ->
      #     through

      #   [^destination_field_on_join_table, ^source_field_on_join_table] ->
      #     through

      #   other ->
      #     raise "The primary key of a join table must be the same as the fields that are used for joining. Needed: #{
      #             inspect([destination_field_on_join_table, source_field_on_join_table])
      #           } got #{other}"
      # end

      through when is_bitstring(through) ->
        through

      _ ->
        raise "`:through` option must be a string representing a join table or a module representinga resource"
    end
  end
end
