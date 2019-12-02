defmodule Ash.Schema do
  defmacro define_schema(name) do
    quote do
      use Ecto.Schema
      @primary_key {:id, :binary_id, autogenerate: true}
      @foreign_key_type :binary_id

      schema unquote(name) do
        for attribute <- @attributes do
          unless attribute.name == :id do
            field(attribute.name, Ash.Schema.type(attribute.type))
          end
        end

        # TODO: Alot of relationship options are ignored here

        for relationship <- Enum.filter(@relationships, &(&1.type == :belongs_to)) do
          belongs_to(relationship.name, relationship.destination,
            define_field: false,
            foreign_key: relationship.source_field,
            references: relationship.destination_field
          )
        end

        for relationship <- Enum.filter(@relationships, &(&1.type == :has_one)) do
          has_one(relationship.name, relationship.destination,
            foreign_key: relationship.destination_field,
            references: relationship.source_field
          )
        end

        for relationship <- Enum.filter(@relationships, &(&1.type == :has_many)) do
          has_many(relationship.name, relationship.destination,
            foreign_key: relationship.destination_field,
            references: relationship.source_field
          )
        end

        for relationship <- Enum.filter(@relationships, &(&1.type == :many_to_many)) do
          many_to_many(relationship.name, relationship.destination,
            join_through: relationship.through,
            join_keys: [
              {relationship.source_field_on_join_table, relationship.source_field},
              {relationship.destination_field_on_join_table, relationship.destination_field}
            ]
          )
        end
      end
    end
  end

  def type(:uuid), do: :binary_id
  def type(other), do: other
end
