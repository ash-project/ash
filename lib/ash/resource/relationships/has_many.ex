defmodule Ash.Resource.Relationships.HasMany do
  @moduledoc "Represents a has_many relationship on a resource"
  defstruct [
    :name,
    :destination,
    :destination_field,
    :private?,
    :source_field,
    :source,
    :writable?,
    :description,
    :expected_cardinality,
    cardinality: :many,
    type: :has_many
  ]

  @type t :: %__MODULE__{
          type: :has_many,
          cardinality: :many,
          expected_cardinality: integer(),
          source: Ash.resource(),
          writable?: boolean,
          name: atom,
          type: Ash.Type.t(),
          destination: Ash.resource(),
          destination_field: atom,
          private?: boolean,
          source_field: atom,
          description: String.t()
        }

  import Ash.Resource.Relationships.SharedOptions
  alias Ash.OptionsHelpers

  @global_opts shared_options()
               |> OptionsHelpers.make_required!(:destination_field)
               |> OptionsHelpers.set_default!(:source_field, :id)

  @opt_schema Ash.OptionsHelpers.merge_schemas(
                [],
                @global_opts,
                "Relationship Options"
              )

  @doc false
  def opt_schema, do: @opt_schema
end
