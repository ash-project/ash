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
    :description,
    :filter,
    :sort,
    :read_action,
    :not_found_message,
    :violation_message,
    :manual,
    :api,
    :writable?,
    no_fields?: false,
    could_be_related_at_creation?: false,
    validate_destination_field?: true,
    cardinality: :many,
    type: :has_many
  ]

  @type t :: %__MODULE__{
          type: :has_many,
          cardinality: :many,
          source: Ash.Resource.t(),
          read_action: atom,
          filter: Ash.Filter.t() | nil,
          no_fields?: boolean,
          name: atom,
          type: Ash.Type.t(),
          writable?: boolean,
          destination: Ash.Resource.t(),
          destination_field: atom,
          private?: boolean,
          source_field: atom,
          description: String.t(),
          manual: atom | {atom, Keyword.t()} | nil
        }

  import Ash.Resource.Relationships.SharedOptions
  alias Ash.OptionsHelpers

  @global_opts shared_options()
               |> OptionsHelpers.set_default!(:source_field, :id)

  @opt_schema Ash.OptionsHelpers.merge_schemas(
                [
                  manual(),
                  no_fields()
                ],
                @global_opts,
                "Relationship Options"
              )

  @doc false
  def opt_schema, do: @opt_schema

  def manual({module, opts}) when is_atom(module) and is_list(opts),
    do: {:ok, {module, opts}}

  def manual(module) when is_atom(module), do: {:ok, {module, []}}
end
