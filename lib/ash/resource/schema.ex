defmodule Ash.Schema do
  @moduledoc false

  # Defines an ecto schema for a resource.

  # This defines struct representation of a resource. Data layers can rely on this
  # schema for persistence.

  defmacro define_schema do
    quote unquote: false do
      alias Ash.Query.Aggregate
      use Ecto.Schema
      @primary_key false

      schema Ash.DataLayer.source(__MODULE__) do
        for relationship <- Ash.Resource.relationships(__MODULE__) do
          @struct_fields {relationship.name,
                          %Ash.NotLoaded{type: :relationship, field: relationship.name}}
        end

        for attribute <- Ash.Resource.attributes(__MODULE__) do
          read_after_writes? = attribute.generated? and is_nil(attribute.default)

          field(attribute.name, Ash.Type.ecto_type(attribute.type),
            primary_key: attribute.primary_key?,
            read_after_writes: read_after_writes?
          )
        end

        field(:aggregates, :map, virtual: true, default: %{})
        field(:calculations, :map, virtual: true, default: %{})

        for aggregate <- Ash.Resource.aggregates(__MODULE__) do
          {:ok, type} = Aggregate.kind_to_type(aggregate.kind)

          field(aggregate.name, Ash.Type.ecto_type(type), virtual: true)
        end
      end
    end
  end
end
