defmodule Ash.Schema do
  @moduledoc false

  # Defines an ecto schema for a resource.

  # This defines struct representation of a resource. Data layers can rely on this
  # schema for persistence.

  defmacro define_schema do
    if Ash.Resource.Info.embedded?(__CALLER__.module) do
      quote unquote: false do
        if Ash.Schema.define?(__MODULE__) do
          alias Ash.Query.Aggregate
          use Ecto.Schema
          @primary_key false

          embedded_schema do
            for attribute <- Ash.Resource.Info.attributes(__MODULE__),
                attribute.name not in Ash.Resource.reserved_names() do
              read_after_writes? = attribute.generated? and is_nil(attribute.default)

              constraint_opts =
                case attribute.type do
                  {:array, _type} ->
                    attribute.constraints[:items] || []

                  _ ->
                    attribute.constraints
                end

              field(
                attribute.name,
                Ash.Type.ecto_type(Ash.Schema.not_a_resource!(attribute.type)),
                Keyword.merge(constraint_opts,
                  primary_key: attribute.primary_key?,
                  read_after_writes: read_after_writes?,
                  redact: attribute.sensitive?,
                  source: attribute.source,
                  default: Ash.Schema.attribute_default(attribute.default)
                )
              )
            end

            field(:aggregates, :map, virtual: true, default: %{})
            field(:calculations, :map, virtual: true, default: %{})
            field(:__metadata__, :map, virtual: true, default: %{}, redact: true)
            field(:__order__, :integer, virtual: true, redact: true)
            field(:__lateral_join_source__, :integer, virtual: true, redact: true)

            struct_fields_name =
              if Module.get_attribute(__MODULE__, :struct_fields) do
                :struct_fields
              else
                :ecto_struct_fields
              end

            Module.register_attribute(__MODULE__, :ash_struct_fields, accumulate: true)

            for field <- Module.get_attribute(__MODULE__, struct_fields_name) do
              Module.put_attribute(__MODULE__, :ash_struct_fields, field)
            end

            for relationship <- Ash.Resource.Info.relationships(__MODULE__),
                relationship.name not in Ash.Resource.reserved_names() do
              Module.put_attribute(
                __MODULE__,
                :ash_struct_fields,
                {relationship.name, %Ash.NotLoaded{type: :relationship, field: relationship.name}}
              )
            end

            for aggregate <- Ash.Resource.Info.aggregates(__MODULE__),
                aggregate.name not in Ash.Resource.reserved_names() do
              {:ok, type, _} =
                if aggregate.kind == :custom do
                  {:ok, aggregate.type, []}
                else
                  Aggregate.kind_to_type(aggregate.kind, :string, [])
                end

              field(
                aggregate.name,
                Ash.Type.ecto_type(Ash.Schema.not_a_resource!(type)),
                virtual: true,
                redact: aggregate.sensitive?
              )

              Module.put_attribute(
                __MODULE__,
                :ash_struct_fields,
                {aggregate.name, %Ash.NotLoaded{type: :aggregate, field: aggregate.name}}
              )
            end

            for calculation <- Ash.Resource.Info.calculations(__MODULE__),
                calculation.name not in Ash.Resource.reserved_names() do
              {mod, _} = calculation.calculation

              field(
                calculation.name,
                Ash.Type.ecto_type(Ash.Schema.not_a_resource!(calculation.type)),
                virtual: true,
                redact: calculation.sensitive?
              )

              Module.put_attribute(
                __MODULE__,
                :ash_struct_fields,
                {calculation.name, %Ash.NotLoaded{type: :calculation, field: calculation.name}}
              )
            end

            struct_fields = Module.get_attribute(__MODULE__, :ash_struct_fields)
            Module.delete_attribute(__MODULE__, struct_fields_name)
            Module.register_attribute(__MODULE__, struct_fields_name, accumulate: true)
            Enum.each(struct_fields, &Module.put_attribute(__MODULE__, struct_fields_name, &1))
          end

          after_compile =
            @after_compile -- [{Ecto.Schema, :__after_compile__}]

          Module.delete_attribute(__MODULE__, :after_compile)
          Module.register_attribute(__MODULE__, :after_compile, accumulate: true)

          for compile_hook <- after_compile do
            @after_compile compile_hook
          end
        end
      end
    else
      quote unquote: false do
        if Ash.Schema.define?(__MODULE__) do
          alias Ash.Query.Aggregate
          use Ecto.Schema
          @primary_key false

          schema Ash.DataLayer.source(__MODULE__) do
            for attribute <- Ash.Resource.Info.attributes(__MODULE__),
                attribute.name not in Ash.Resource.reserved_names() do
              read_after_writes? = attribute.generated? and is_nil(attribute.default)

              constraint_opts =
                case attribute.type do
                  {:array, _type} ->
                    attribute.constraints[:items] || []

                  _ ->
                    attribute.constraints
                end

              field(
                attribute.name,
                Ash.Type.ecto_type(Ash.Schema.not_a_resource!(attribute.type)),
                Keyword.merge(constraint_opts,
                  primary_key: attribute.primary_key?,
                  read_after_writes: read_after_writes?,
                  redact: attribute.sensitive?,
                  source: attribute.source,
                  default: Ash.Schema.attribute_default(attribute.default)
                )
              )
            end

            field(:aggregates, :map, virtual: true, default: %{})
            field(:calculations, :map, virtual: true, default: %{})
            field(:__metadata__, :map, virtual: true, default: %{}, redact: true)
            field(:__order__, :integer, virtual: true, redact: true)
            field(:__lateral_join_source__, :integer, virtual: true, redact: true)

            struct_fields_name =
              if Module.get_attribute(__MODULE__, :struct_fields) do
                :struct_fields
              else
                :ecto_struct_fields
              end

            Module.register_attribute(__MODULE__, :ash_struct_fields, accumulate: true)

            for field <- Module.get_attribute(__MODULE__, struct_fields_name) do
              Module.put_attribute(__MODULE__, :ash_struct_fields, field)
            end

            for relationship <- Ash.Resource.Info.relationships(__MODULE__),
                relationship.name not in Ash.Resource.reserved_names() do
              case relationship do
                %{no_attributes?: true} ->
                  :ok

                %{manual?: true} ->
                  :ok

                %{manual: manual} when not is_nil(manual) ->
                  :ok

                %{type: :belongs_to} ->
                  belongs_to relationship.name, relationship.destination,
                    define_field: false,
                    references: relationship.destination_attribute,
                    foreign_key: relationship.source_attribute

                %{type: :has_many} ->
                  has_many relationship.name, relationship.destination,
                    foreign_key: relationship.destination_attribute,
                    references: relationship.source_attribute

                %{type: :has_one} ->
                  has_one relationship.name, relationship.destination,
                    foreign_key: relationship.destination_attribute,
                    references: relationship.source_attribute

                %{type: :many_to_many} ->
                  many_to_many relationship.name, relationship.destination,
                    join_through: relationship.through,
                    join_keys: [
                      {relationship.source_attribute_on_join_resource,
                       relationship.source_attribute},
                      {relationship.destination_attribute_on_join_resource,
                       relationship.destination_attribute}
                    ]
              end

              Module.put_attribute(
                __MODULE__,
                :ash_struct_fields,
                {relationship.name, %Ash.NotLoaded{type: :relationship, field: relationship.name}}
              )
            end

            for aggregate <- Ash.Resource.Info.aggregates(__MODULE__),
                aggregate.name not in Ash.Resource.reserved_names() do
              {:ok, type, _} =
                if aggregate.kind == :custom do
                  {:ok, aggregate.type, []}
                else
                  Aggregate.kind_to_type(aggregate.kind, :string, [])
                end

              field(
                aggregate.name,
                Ash.Type.ecto_type(Ash.Schema.not_a_resource!(type)),
                virtual: true,
                redact: aggregate.sensitive?
              )

              Module.put_attribute(
                __MODULE__,
                :ash_struct_fields,
                {aggregate.name, %Ash.NotLoaded{type: :aggregate, field: aggregate.name}}
              )
            end

            for calculation <- Ash.Resource.Info.calculations(__MODULE__),
                calculation.name not in Ash.Resource.reserved_names() do
              {mod, _} = calculation.calculation

              field(
                calculation.name,
                Ash.Type.ecto_type(Ash.Schema.not_a_resource!(calculation.type)),
                virtual: true,
                redact: calculation.sensitive?
              )

              Module.put_attribute(
                __MODULE__,
                :ash_struct_fields,
                {calculation.name, %Ash.NotLoaded{type: :calculation, field: calculation.name}}
              )
            end

            struct_fields = Module.get_attribute(__MODULE__, :ash_struct_fields)

            Module.delete_attribute(__MODULE__, struct_fields_name)
            Module.register_attribute(__MODULE__, struct_fields_name, accumulate: true)
            Enum.each(struct_fields, &Module.put_attribute(__MODULE__, struct_fields_name, &1))
          end

          unless Kernel.Typespec.defines_type?(__MODULE__, {:t, 0}) do
            @type t :: %__MODULE__{}
          end

          after_compile =
            @after_compile -- [{Ecto.Schema, :__after_compile__}]

          Module.delete_attribute(__MODULE__, :after_compile)
          Module.register_attribute(__MODULE__, :after_compile, accumulate: true)

          for compile_hook <- after_compile do
            @after_compile compile_hook
          end
        end
      end
    end
  end

  def define?(resource) do
    has_action?(resource) or !Enum.empty?(Ash.Resource.Info.fields(resource))
  end

  defp has_action?(resource) do
    resource
    |> Ash.Resource.Info.actions()
    |> Enum.any?(&(&1.type != :action))
  end

  @doc false
  def not_a_resource!({:array, type}) do
    {:array, not_a_resource!(type)}
  end

  def not_a_resource!(module) when is_atom(module) do
    type =
      if Ash.Type.NewType.new_type?(module) do
        Ash.Type.NewType.subtype_of(module)
      else
        module
      end

    if Ash.Resource.Info.resource?(type) && !Ash.Resource.Info.embedded?(type) do
      raise """
      Non-embedded resources can not be used as types.

      Got #{inspect(module)}

      To use them as a type, instead use the `:struct` type, with the `instance_of` constraint.

      For example:

          attribute :foo, :struct, constraints: [instance_of: #{inspect(module)}]

      Or as an array:

          attribute :foo, {:array, :struct}, constraints: [items: [instance_of: #{inspect(module)}]]

      You can disable this warning by setting `config :ash, allow_resources_as_types: true` in your config.

      In 3.0, the flag to disable this behaviour will not be available.
      """
    else
      module
    end
  end

  def not_a_resource!(type), do: type

  def attribute_default(fun) when is_function(fun), do: nil
  def attribute_default({_mod, _func, _args}), do: nil
  def attribute_default(value), do: value
end
