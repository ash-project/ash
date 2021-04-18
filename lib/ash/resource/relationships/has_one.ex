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
    :context,
    :description,
    :not_found_message,
    :violation_message,
    cardinality: :one,
    type: :has_one
  ]

  @type t :: %__MODULE__{
          type: :has_one,
          cardinality: :one,
          source: Ash.Resource.t(),
          writable?: boolean,
          name: atom,
          type: Ash.Type.t(),
          destination: Ash.Resource.t(),
          destination_field: atom,
          private?: boolean,
          source_field: atom,
          allow_orphans?: boolean,
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
