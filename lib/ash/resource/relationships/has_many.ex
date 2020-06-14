defmodule Ash.Resource.Relationships.HasMany do
  @moduledoc false
  defstruct [
    :name,
    :destination,
    :destination_field,
    :source_field,
    :source,
    :reverse_relationship,
    cardinality: :many,
    type: :has_many
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
