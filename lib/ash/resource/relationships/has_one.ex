defmodule Ash.Resource.Relationships.HasOne do
  @moduledoc false
  defstruct [
    :name,
    :source,
    :destination,
    :destination_field,
    :source_field,
    :reverse_relationship,
    :allow_orphans?,
    cardinality: :one,
    type: :has_one
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
