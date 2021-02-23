defmodule Ash.Resource.Aggregate do
  @moduledoc "Represents a named aggregate on the resource that can be loaded"
  defstruct [:name, :relationship_path, :filter, :kind, :description, :private?, :field, :sort]

  @schema [
    name: [
      type: :atom,
      doc: "The field to place the aggregate in",
      required: true
    ],
    relationship_path: [
      type: {:custom, __MODULE__, :relationship_path, []},
      doc: "The relationship or relationship path to use for the aggregate",
      required: true
    ],
    kind: [
      type: {:in, [:count, :first]},
      doc: "The kind of the aggregate",
      required: true
    ],
    field: [
      type: :atom,
      doc:
        "The field to aggregate. Defaults to the first field in the primary key of the resource",
      required: false
    ],
    filter: [
      type: :keyword_list,
      doc: "A filter to apply to the aggregate",
      default: []
    ],
    sort: [
      type: :any,
      doc: "A sort to be applied to the aggregate"
    ],
    description: [
      type: :string,
      doc: "An optional description for the aggregate"
    ],
    private?: [
      type: :boolean,
      default: false,
      doc:
        "Whether or not the aggregate will appear in any interfaces created off of this resource, e.g AshJsonApi and AshGraphql"
    ]
  ]

  @type t :: %__MODULE__{
          name: atom(),
          relationship_path: {:ok, list(atom())} | {:error, String.t()},
          filter: Keyword.t(),
          field: atom,
          kind: :count | :first,
          description: String.t() | nil,
          private?: boolean
        }

  @doc false
  def schema, do: @schema

  def relationship_path(value) do
    value = List.wrap(value)

    if Enum.all?(value, &is_atom/1) do
      {:ok, value}
    else
      {:error, "relationship path must be atoms"}
    end
  end
end
