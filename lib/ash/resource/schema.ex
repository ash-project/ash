defmodule Ash.Schema do
  @moduledoc false

  # Defines an ecto schema for a resource.

  # This defines struct representation of a resource. Data layers can rely on this
  # schema for persistence.

  defmacro define_schema do
    quote unquote: false do
      use Ecto.Schema
      @primary_key false

      schema "" do
        for attribute <- Ash.attributes(__MODULE__) do
          read_after_writes? = attribute.generated? and is_nil(attribute.default)

          field(attribute.name, Ash.Type.ecto_type(attribute.type),
            primary_key: attribute.primary_key?,
            read_after_writes: read_after_writes?
          )
        end

        relationships = Ash.relationships(__MODULE__)

        for relationship <- Enum.filter(relationships, &(&1.type == :belongs_to)) do
          belongs_to(relationship.name, relationship.destination,
            define_field: false,
            foreign_key: relationship.source_field,
            references: relationship.destination_field
          )
        end

        for relationship <- Enum.filter(relationships, &(&1.type == :has_one)) do
          has_one(relationship.name, relationship.destination,
            foreign_key: relationship.destination_field,
            references: relationship.source_field
          )
        end

        for relationship <- Enum.filter(relationships, &(&1.type == :has_many)) do
          has_many(relationship.name, relationship.destination,
            foreign_key: relationship.destination_field,
            references: relationship.source_field
          )
        end

        for relationship <- Enum.filter(relationships, &(&1.type == :many_to_many)) do
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
end
