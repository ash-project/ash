defmodule Ash.Resource.Relationships.HasMany do
  @moduledoc "Represents a has_many relationship on a resource"
  defstruct [
    :name,
    :destination,
    :destination_attribute,
    :public?,
    :source_attribute,
    :source,
    :context,
    :description,
    :filter,
    :sort,
    :default_sort,
    :read_action,
    :not_found_message,
    :violation_message,
    :manual,
    :domain,
    :writable?,
    :autogenerated_join_relationship_of,
    filterable?: true,
    filters: [],
    sortable?: true,
    no_attributes?: false,
    allow_forbidden_field?: false,
    authorize_read_with: :filter,
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
          filters: list(any),
          no_attributes?: boolean,
          name: atom,
          type: Ash.Type.t(),
          writable?: boolean,
          destination: Ash.Resource.t(),
          destination_attribute: atom,
          autogenerated_join_relationship_of: atom | nil,
          public?: boolean,
          filterable?: boolean,
          sortable?: true,
          source_attribute: atom,
          sort: Keyword.t() | nil,
          default_sort: Keyword.t() | nil,
          description: String.t(),
          manual: atom | {atom, Keyword.t()} | nil
        }

  import Ash.Resource.Relationships.SharedOptions

  @global_opts shared_options()
               |> Spark.Options.Helpers.set_default!(:source_attribute, :id)

  @opt_schema Spark.Options.merge(
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

  def transform(relationship) do
    {:ok, relationship |> Ash.Resource.Actions.Read.concat_filters()}
  end
end
