# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Schema do
  @moduledoc false

  # Defines an ecto schema for a resource.

  # This defines struct representation of a resource. Data layers can rely on this
  # schema for persistence.
  @old_ecto_attr (case :application.get_key(:ecto, :vsn) do
                    {:ok, version} ->
                      !Version.match?(List.to_string(version), ">= 3.13.0")

                    _ ->
                      false
                  end)

  # 4.0 *all attributes* need to default to `%Ash.NotLoaded{}`
  # that way if you do `%Post{id: id}` you get `%Post{id: id, title: %Ash.NotLoaded{}, body: %Ash.NotLoaded{}}`
  # for example
  defmacro define_schema do
    if Ash.Resource.Info.embedded?(__CALLER__.module) do
      quote unquote: false, bind_quoted: [old_ecto_attr?: @old_ecto_attr] do
        if Ash.Schema.define?(__MODULE__) do
          alias Ash.Query.Aggregate
          use Ecto.Schema
          @primary_key false

          if old_ecto_attr? do
            @ecto_derive_inspect_for_redacted_fields false
          else
            @derive_inspect_for_redacted_fields false
          end

          embedded_schema do
            for attribute <- Ash.Resource.Info.attributes(__MODULE__),
                attribute.name not in Ash.Resource.reserved_names() do
              read_after_writes? = attribute.generated?

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

            Ash.Schema.register_ecto_autogenerate(__MODULE__)

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

              constraint_opts =
                case calculation.type do
                  {:array, _type} ->
                    calculation.constraints[:items] || []

                  _ ->
                    calculation.constraints
                end

              field(
                calculation.name,
                Ash.Type.ecto_type(Ash.Schema.not_a_resource!(calculation.type)),
                Keyword.merge(constraint_opts,
                  virtual: true,
                  redact: calculation.sensitive?
                )
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
      quote unquote: false, bind_quoted: [old_ecto_attr?: @old_ecto_attr] do
        if Ash.Schema.define?(__MODULE__) do
          alias Ash.Query.Aggregate
          use Ecto.Schema
          @primary_key false

          if old_ecto_attr? do
            @ecto_derive_inspect_for_redacted_fields false
          else
            @derive_inspect_for_redacted_fields false
          end

          schema Ash.DataLayer.source(__MODULE__) do
            for attribute <- Ash.Resource.Info.attributes(__MODULE__),
                attribute.name not in Ash.Resource.reserved_names() do
              read_after_writes? = attribute.generated?

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

            Ash.Schema.register_ecto_autogenerate(__MODULE__)

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

              constraint_opts =
                case calculation.type do
                  {:array, _type} ->
                    calculation.constraints[:items] || []

                  _ ->
                    calculation.constraints
                end

              field(
                calculation.name,
                Ash.Type.ecto_type(Ash.Schema.not_a_resource!(calculation.type)),
                Keyword.merge(constraint_opts,
                  virtual: true,
                  redact: calculation.sensitive?
                )
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

          if !Kernel.Typespec.defines_type?(__MODULE__, {:t, 0}) do
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
      """
    else
      module
    end
  end

  def not_a_resource!(type), do: type

  def attribute_default(fun) when is_function(fun), do: nil
  def attribute_default({_mod, _func, _args}), do: nil
  def attribute_default(value), do: value

  @doc false
  # Called at compile time from within the `schema do ... end` block to wire
  # Ash's create_timestamp/update_timestamp attributes into Ecto's autogenerate
  # system. This enables Repo.insert/1 and Repo.update/1 to auto-populate
  # timestamp fields without going through Ash's changeset pipeline.
  def register_ecto_autogenerate(module) do
    autogen_timestamps =
      Ash.Resource.Info.attributes(module)
      |> Enum.filter(&timestamp_attribute?/1)

    # Partition by match_other_defaults?: timestamps that share defaults are
    # grouped into a single {[field1, field2], mfa} tuple so Ecto calls the MFA
    # once and assigns the same value to all fields. Timestamps with
    # match_other_defaults? == false each get their own entry.
    {shared, individual} =
      Enum.split_with(autogen_timestamps, & &1.match_other_defaults?)

    shared
    |> Enum.group_by(&Ash.Type.storage_type(&1.type, &1.constraints))
    |> Enum.each(fn {storage_type, attrs} ->
      field_names = Enum.map(attrs, & &1.name)

      Module.put_attribute(
        module,
        :ecto_autogenerate,
        {field_names, {Ecto.Schema, :__timestamps__, [storage_type]}}
      )
    end)

    Enum.each(individual, fn attr ->
      storage_type = Ash.Type.storage_type(attr.type, attr.constraints)

      Module.put_attribute(
        module,
        :ecto_autogenerate,
        {[attr.name], {Ecto.Schema, :__timestamps__, [storage_type]}}
      )
    end)

    # Register update_timestamp fields in :ecto_autoupdate so Repo.update/1
    # refreshes them automatically.
    update_timestamps =
      Enum.filter(autogen_timestamps, fn attr -> not is_nil(attr.update_default) end)

    {shared_update, individual_update} =
      Enum.split_with(update_timestamps, & &1.match_other_defaults?)

    shared_update
    |> Enum.group_by(&Ash.Type.storage_type(&1.type, &1.constraints))
    |> Enum.each(fn {storage_type, attrs} ->
      field_names = Enum.map(attrs, & &1.name)

      Module.put_attribute(
        module,
        :ecto_autoupdate,
        {field_names, {Ecto.Schema, :__timestamps__, [storage_type]}}
      )
    end)

    Enum.each(individual_update, fn attr ->
      storage_type = Ash.Type.storage_type(attr.type, attr.constraints)

      Module.put_attribute(
        module,
        :ecto_autoupdate,
        {[attr.name], {Ecto.Schema, :__timestamps__, [storage_type]}}
      )
    end)
  end

  @doc false
  def timestamp_attribute?(attr) do
    ecto_datetime_type?(Ash.Type.storage_type(attr.type, attr.constraints)) &&
      attr.writable? == false &&
      datetime_default?(attr.default) &&
      attr.allow_nil? == false
  end

  defp datetime_default?(fun) when is_function(fun, 0) do
    fun == (&DateTime.utc_now/0)
  end

  defp datetime_default?(_), do: false

  defp ecto_datetime_type?(type) do
    type in [:utc_datetime, :utc_datetime_usec, :naive_datetime, :naive_datetime_usec]
  end
end
