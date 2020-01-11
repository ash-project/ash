defmodule Ash.Resource.Relationships.BelongsTo do
  @moduledoc "The representation of a `belongs_to` relationship"
  defstruct [
    :name,
    :cardinality,
    :type,
    :destination,
    :primary_key?,
    :define_field?,
    :field_type,
    :destination_field,
    :source_field,
    :source,
    :reverse_relationship,
    :write_rules
  ]

  @type t :: %__MODULE__{
          type: :belongs_to,
          cardinality: :one,
          name: atom,
          source: Ash.resource(),
          destination: Ash.resource(),
          primary_key?: boolean,
          define_field?: boolean,
          field_type: Ash.Type.t(),
          destination_field: atom,
          source_field: atom | nil,
          write_rules: Keyword.t()
        }

  @opt_schema Ashton.schema(
                opts: [
                  destination_field: :atom,
                  source_field: :atom,
                  primary_key?: :boolean,
                  define_field?: :boolean,
                  field_type: :atom,
                  reverse_relationship: :atom,
                  write_rules: :keyword
                ],
                defaults: [
                  destination_field: :id,
                  primary_key?: false,
                  define_field?: true,
                  field_type: :uuid,
                  write_rules: []
                ],
                describe: [
                  reverse_relationship:
                    "A requirement for side loading data. Must be the name of an inverse relationship on the destination resource.",
                  define_field?:
                    "If set to `false` a field is not created on the resource for this relationship, and one must be manually added in `attributes`.",
                  field_type: "The field type of the automatically created field.",
                  destination_field:
                    "The field on the related resource that should match the `source_field` on this resource.",
                  source_field:
                    "The field on this resource that should match the `destination_field` on the related resource.  Default: [relationship_name]_id",
                  primary_key?:
                    "Whether this field is, or is part of, the primary key of a resource.",
                  write_rules: """
                  Steps applied on an relationship during create or update. If no steps are defined, authorization to change will fail.
                  If set to false, no steps are applied and any changes are allowed (assuming the action was authorized as a whole)
                  Remember that any changes against the destination records *will* still be authorized regardless of this setting.
                  """
                ]
              )

  @doc false
  def opt_schema(), do: @opt_schema

  @spec new(
          resource :: Ash.resource(),
          name :: atom,
          related_resource :: Ash.resource(),
          opts :: Keyword.t()
        ) :: {:ok, t()} | {:error, term}
  def new(resource, name, related_resource, opts \\ []) do
    # Don't call functions on the resource! We don't want it to compile here
    case Ashton.validate(opts, @opt_schema) do
      {:ok, opts} ->
        write_rules =
          case opts[:write_rules] do
            false ->
              false

            steps ->
              base_attribute_opts = [
                relationship_name: name,
                destination: related_resource,
                resource: resource
              ]

              Enum.map(steps, fn {step, {mod, opts}} ->
                {step, {mod, Keyword.merge(base_attribute_opts, opts)}}
              end)
          end

        {:ok,
         %__MODULE__{
           name: name,
           write_rules: write_rules,
           source: resource,
           type: :belongs_to,
           cardinality: :one,
           field_type: opts[:field_type],
           define_field?: opts[:define_field?],
           primary_key?: opts[:primary_key?],
           destination: related_resource,
           destination_field: opts[:destination_field],
           source_field: opts[:source_field] || :"#{name}_id",
           reverse_relationship: opts[:reverse_relationship]
         }}

      {:error, error} ->
        {:error, error}
    end
  end
end
