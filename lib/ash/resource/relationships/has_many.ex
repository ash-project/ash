defmodule Ash.Resource.Relationships.HasMany do
  @moduledoc "Represents a has_many relationship on a resource"
  defstruct [
    :name,
    :destination,
    :destination_attribute,
    :private?,
    :source_attribute,
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
    no_attributes?: false,
    could_be_related_at_creation?: false,
    validate_destination_attribute?: true,
    cardinality: :many,
    type: :has_many
  ]

  @type t :: %__MODULE__{
          type: :has_many,
          cardinality: :many,
          source: Ash.Resource.t(),
          read_action: atom,
          filter: Ash.Filter.t() | nil,
          no_attributes?: boolean,
          name: atom,
          type: Ash.Type.t(),
          writable?: boolean,
          destination: Ash.Resource.t(),
          destination_attribute: atom,
          private?: boolean,
          source_attribute: atom,
          description: String.t(),
          manual: atom | {atom, Keyword.t()} | nil
        }

  import Ash.Resource.Relationships.SharedOptions
  alias Spark.OptionsHelpers

  @global_opts shared_options()
               |> OptionsHelpers.set_default!(:source_attribute, :id)

  @opt_schema Spark.OptionsHelpers.merge_schemas(
                [
                  manual(),
                  no_attributes()
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
