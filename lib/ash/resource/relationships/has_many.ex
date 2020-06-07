defmodule Ash.Resource.Relationships.HasMany do
  @moduledoc false
  defstruct [
    :name,
    :type,
    :cardinality,
    :destination,
    :destination_field,
    :source_field,
    :source,
    :reverse_relationship
  ]

  @type t :: %__MODULE__{
          type: :has_many,
          cardinality: :many,
          source: Ash.resource(),
          name: atom,
          type: Ash.Type.t(),
          destination: Ash.resource(),
          destination_field: atom,
          source_field: atom,
          reverse_relationship: atom() | nil
        }

  import Ash.Resource.Relationships.SharedOptions

  @global_opts shared_options()
               |> make_required!(:destination_field)
               |> set_default!(:source_field, :id)

  @opt_schema Ash.OptionsHelpers.merge_schemas(
                [],
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
           type: :has_many,
           cardinality: :many,
           destination: related_resource,
           destination_field: opts[:destination_field],
           source_field: opts[:source_field],
           reverse_relationship: opts[:reverse_relationship]
         }}

      {:error, error} ->
        {:error, error}
    end
  end
end
