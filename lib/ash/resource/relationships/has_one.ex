defmodule Ash.Resource.Relationships.HasOne do
  @doc false
  defstruct [
    :name,
    :type,
    :cardinality,
    :destination,
    :destination_field,
    :source_field
  ]

  @type t :: %__MODULE__{
          type: :has_one,
          cardinality: :one,
          name: atom,
          type: Ash.Type.t(),
          destination: Ash.resource(),
          destination_field: atom,
          source_field: atom
        }

  @opt_schema Ashton.schema(
                opts: [
                  destination_field: :atom,
                  source_field: :atom
                ],
                defaults: [
                  source_field: :id
                ],
                describe: [
                  destination_field:
                    "The field on the related resource that should match the `source_field` on this resource. Default: [resource.name]_id",
                  source_field:
                    "The field on this resource that should match the `destination_field` on the related resource."
                ]
              )

  @doc false
  def opt_schema(), do: @opt_schema

  @spec new(
          resource_type :: String.t(),
          name :: atom,
          related_resource :: Ash.resource(),
          opts :: Keyword.t()
        ) :: {:ok, t()} | {:error, term}
  @doc false
  def new(resource_type, name, related_resource, opts \\ []) do
    case Ashton.validate(opts, @opt_schema) do
      {:ok, opts} ->
        {:ok,
         %__MODULE__{
           name: name,
           type: :has_one,
           cardinality: :one,
           destination: related_resource,
           destination_field: opts[:destination_field] || :"#{resource_type}_id",
           source_field: opts[:source_field]
         }}

      {:error, errors} ->
        {:error, errors}
    end
  end
end
