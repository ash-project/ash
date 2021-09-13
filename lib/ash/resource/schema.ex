defmodule Ash.Schema do
  @moduledoc false

  # Defines an ecto schema for a resource.

  # This defines struct representation of a resource. Data layers can rely on this
  # schema for persistence.

  defmacro define_schema do
    if Ash.Resource.Info.embedded?(__CALLER__.module) do
      quote unquote: false do
        alias Ash.Query.Aggregate
        use Ecto.Schema
        @primary_key false

        embedded_schema do
          for relationship <- Ash.Resource.Info.relationships(__MODULE__) do
            @struct_fields {relationship.name,
                            %Ash.NotLoaded{type: :relationship, field: relationship.name}}
          end

          for attribute <- Ash.Resource.Info.attributes(__MODULE__) do
            read_after_writes? = attribute.generated? and is_nil(attribute.default)

            field(
              attribute.name,
              Ash.Type.ecto_type(attribute.type),
              Keyword.merge(
                [
                  primary_key: attribute.primary_key?,
                  read_after_writes: read_after_writes?,
                  redact: attribute.sensitive?
                ],
                attribute.constraints
              )
            )
          end

          field(:aggregates, :map, virtual: true, default: %{})
          field(:calculations, :map, virtual: true, default: %{})
          field(:__metadata__, :map, virtual: true, default: %{}, redact: true)
          field(:__order__, :integer, virtual: true)

          for aggregate <- Ash.Resource.Info.aggregates(__MODULE__) do
            {:ok, type} = Aggregate.kind_to_type(aggregate.kind, :string)

            field(aggregate.name, Ash.Type.ecto_type(type), virtual: true)

            struct_fields = Keyword.delete(@struct_fields, aggregate.name)
            Module.delete_attribute(__MODULE__, :struct_fields)
            Module.register_attribute(__MODULE__, :struct_fields, accumulate: true)
            Enum.each(struct_fields, &Module.put_attribute(__MODULE__, :struct_fields, &1))

            @struct_fields {aggregate.name,
                            %Ash.NotLoaded{type: :aggregate, field: aggregate.name}}
          end

          for calculation <- Ash.Resource.Info.calculations(__MODULE__) do
            {mod, _} = calculation.calculation

            field(calculation.name, Ash.Type.ecto_type(calculation.type), virtual: true)

            struct_fields = Keyword.delete(@struct_fields, calculation.name)
            Module.delete_attribute(__MODULE__, :struct_fields)
            Module.register_attribute(__MODULE__, :struct_fields, accumulate: true)
            Enum.each(struct_fields, &Module.put_attribute(__MODULE__, :struct_fields, &1))

            @struct_fields {calculation.name,
                            %Ash.NotLoaded{type: :calculation, field: calculation.name}}
          end
        end
      end
    else
      quote unquote: false do
        alias Ash.Query.Aggregate
        use Ecto.Schema
        @primary_key false

        schema Ash.DataLayer.source(__MODULE__) do
          for relationship <- Ash.Resource.Info.relationships(__MODULE__) do
            @struct_fields {relationship.name,
                            %Ash.NotLoaded{type: :relationship, field: relationship.name}}
          end

          for attribute <- Ash.Resource.Info.attributes(__MODULE__) do
            read_after_writes? = attribute.generated? and is_nil(attribute.default)

            field(
              attribute.name,
              Ash.Type.ecto_type(attribute.type),
              Keyword.merge(
                [
                  primary_key: attribute.primary_key?,
                  read_after_writes: read_after_writes?,
                  redact: attribute.sensitive?
                ],
                attribute.constraints
              )
            )
          end

          field(:aggregates, :map, virtual: true, default: %{})
          field(:calculations, :map, virtual: true, default: %{})
          field(:__metadata__, :map, virtual: true, default: %{}, redact: true)
          field(:__order__, :integer, virtual: true)

          for aggregate <- Ash.Resource.Info.aggregates(__MODULE__) do
            {:ok, type} = Aggregate.kind_to_type(aggregate.kind, :string)

            field(aggregate.name, Ash.Type.ecto_type(type), virtual: true)

            struct_fields = Keyword.delete(@struct_fields, aggregate.name)
            Module.delete_attribute(__MODULE__, :struct_fields)
            Module.register_attribute(__MODULE__, :struct_fields, accumulate: true)
            Enum.each(struct_fields, &Module.put_attribute(__MODULE__, :struct_fields, &1))

            @struct_fields {aggregate.name,
                            %Ash.NotLoaded{type: :aggregate, field: aggregate.name}}
          end

          for calculation <- Ash.Resource.Info.calculations(__MODULE__) do
            {mod, _} = calculation.calculation

            field(calculation.name, Ash.Type.ecto_type(calculation.type), virtual: true)

            struct_fields = Keyword.delete(@struct_fields, calculation.name)
            Module.delete_attribute(__MODULE__, :struct_fields)
            Module.register_attribute(__MODULE__, :struct_fields, accumulate: true)
            Enum.each(struct_fields, &Module.put_attribute(__MODULE__, :struct_fields, &1))

            @struct_fields {calculation.name,
                            %Ash.NotLoaded{type: :calculation, field: calculation.name}}
          end
        end
      end
    end
  end
end
