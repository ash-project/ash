defmodule Ash.Resource.Relationships.BelongsTo do
  @moduledoc false
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
    :reverse_relationship
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
          source_field: atom | nil
        }

  import Ash.Resource.Relationships.SharedOptions

  @global_opts shared_options()
               |> set_default!(:destination_field, :id)

  @opt_schema Ash.OptionsHelpers.merge_schemas(
                [
                  primary_key?: [
                    type: :boolean,
                    default: false,
                    doc: "Whether this field is, or is part of, the primary key of a resource."
                  ],
                  define_field?: [
                    type: :boolean,
                    default: true,
                    doc:
                      "If set to `false` a field is not created on the resource for this relationship, and one must be manually added in `attributes`."
                  ],
                  field_type: [
                    type: :any,
                    default: :uuid,
                    doc: "The field type of the automatically created field."
                  ]
                ],
                @global_opts,
                "Relationship Options"
              )

  @doc false
  def opt_schema, do: @opt_schema

  @spec new(
          resource :: Ash.resource(),
          name :: atom,
          related_resource :: Ash.resource(),
          opts :: Keyword.t()
        ) :: {:ok, t()} | {:error, term}

  # sobelow_skip ["DOS.BinToAtom"]
  def new(resource, name, related_resource, opts \\ []) do
    # Don't call functions on the resource! We don't want it to compile here

    case NimbleOptions.validate(opts, @opt_schema) do
      {:ok, opts} ->
        {:ok,
         %__MODULE__{
           name: name,
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
