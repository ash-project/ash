defmodule Ash.Resource.Relationships.HasMany do
  @moduledoc "Represents a has_many relationship on a resource"
  defstruct [
    :name,
    :destination,
    :destination_field,
    :private?,
    :source_field,
    :source,
    :context,
    :writable?,
    :description,
    :filter,
    :read_action,
    :not_found_message,
    :violation_message,
    cardinality: :many,
    type: :has_many
  ]

  @type t :: %__MODULE__{
          type: :has_many,
          cardinality: :many,
          source: Ash.Resource.t(),
          writable?: boolean,
          read_action: atom,
          filter: Ash.Filter.t(),
          name: atom,
          type: Ash.Type.t(),
          destination: Ash.Resource.t(),
          destination_field: atom,
          private?: boolean,
          source_field: atom,
          description: String.t()
        }

  import Ash.Resource.Relationships.SharedOptions
  alias Ash.OptionsHelpers

  @global_opts shared_options()
               |> OptionsHelpers.set_default!(:source_field, :id)

  @opt_schema Ash.OptionsHelpers.merge_schemas(
                [],
                @global_opts,
                "Relationship Options"
              )

  @doc false
  def opt_schema, do: @opt_schema
end
