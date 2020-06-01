defmodule Ash.Resource.Relationships.HasOne do
  @doc false
  defstruct [
    :name,
    :type,
    :source,
    :cardinality,
    :destination,
    :destination_field,
    :source_field,
    :reverse_relationship,
    :allow_orphans?
  ]

  @type t :: %__MODULE__{
          type: :has_one,
          cardinality: :one,
          source: Ash.resource(),
          name: atom,
          type: Ash.Type.t(),
          destination: Ash.resource(),
          destination_field: atom,
          source_field: atom,
          allow_orphans?: boolean,
          reverse_relationship: atom | nil
        }

  @opt_schema [
    destination_field: [
      type: :atom,
      doc:
        "The field on the related resource that should match the `source_field` on this resource. Default: [resource.name]_id"
    ],
    source_field: [
      type: :atom,
      default: :id,
      doc:
        "The field on this resource that should match the `destination_field` on the related resource."
    ],
    reverse_relationship: [
      type: :atom,
      doc:
        "A requirement for side loading data. Must be the name of an inverse relationship on the destination resource."
    ]
  ]

  @doc false
  def opt_schema(), do: @opt_schema

  @spec new(
          resource :: Ash.resource(),
          resource_type :: String.t(),
          name :: atom,
          related_resource :: Ash.resource(),
          opts :: Keyword.t()
        ) :: {:ok, t()} | {:error, term}
  @doc false
  def new(resource, resource_type, name, related_resource, opts \\ []) do
    # Don't call functions on the resource! We don't want it to compile here
    case NimbleOptions.validate(opts, @opt_schema) do
      {:ok, opts} ->
        {:ok,
         %__MODULE__{
           name: name,
           source: resource,
           type: :has_one,
           cardinality: :one,
           destination: related_resource,
           destination_field: opts[:destination_field] || :"#{resource_type}_id",
           source_field: opts[:source_field],
           reverse_relationship: opts[:reverse_relationship]
         }}

      {:error, errors} ->
        {:error, errors}
    end
  end
end
