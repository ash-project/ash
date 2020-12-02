defmodule Ash.Resource.Relationships.HasOne do
  @moduledoc "Represents a has_one relationship on a resource"

  defstruct [
    :name,
    :source,
    :destination,
    :destination_field,
    :private?,
    :source_field,
    :allow_orphans?,
    :writable?,
    :description,
    cardinality: :one,
    expected_cardinality: 1,
    type: :has_one
  ]

  @type t :: %__MODULE__{
          type: :has_one,
          cardinality: :one,
          expected_cardinality: integer(),
          source: Ash.resource(),
          writable?: boolean,
          name: atom,
          type: Ash.Type.t(),
          destination: Ash.resource(),
          destination_field: atom,
          private?: boolean,
          source_field: atom,
          allow_orphans?: boolean,
          description: String.t()
        }

  alias Ash.OptionsHelpers
  import Ash.Resource.Relationships.SharedOptions

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
