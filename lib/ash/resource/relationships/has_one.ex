defmodule Ash.Resource.Relationships.HasOne do
  @doc false
  defstruct [
    :name,
    :type,
    :cardinality,
    :destination,
    :primary_key?,
    :destination_field,
    :source_field
  ]

  @type t :: %__MODULE__{
          type: :has_one,
          cardinality: :one
        }

  @opt_schema Ashton.schema(
                opts: [
                  destination_field: :atom,
                  source_field: :atom,
                  primary_key?: :boolean
                ],
                defaults: [
                  source_field: :id,
                  primary_key?: false
                ],
                describe: [
                  destination_field:
                    "The field on the related resource that should match the `source_field` on this resource. Default: <resource.name>_id",
                  source_field:
                    "The field on this resource that should match the `destination_field` on the related resource.",
                  primary_key?:
                    "Whether this field is, or is part of, the primary key of a resource."
                ]
              )

  @doc false
  def opt_schema(), do: @opt_schema

  @spec new(
          resource_name :: String.t(),
          name :: atom,
          related_resource :: Ash.resource(),
          opts :: Keyword.t()
        ) :: t()
  @doc false
  def new(resource_name, name, related_resource, opts \\ []) do
    opts =
      case Ashton.validate(opts, @opt_schema) do
        {:ok, opts} ->
          {:ok,
           %__MODULE__{
             name: name,
             type: :has_one,
             cardinality: :one,
             destination: related_resource,
             primary_key?: opts[:primary_key?],
             destination_field: opts[:destination_field] || :"#{resource_name}_id",
             source_field: opts[:source_field]
           }}

        {:error, errors} ->
          {:error, errors}
      end
  end
end
