defmodule Ash.Resource.Aggregate do
  @moduledoc "Represents a named aggregate on the resource that can be loaded"
  defstruct [:name, :relationship_path, :filter, :kind, :description]

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
      type: {:one_of, Ash.Query.Aggregate.kinds()},
      doc: "The kind of the aggregate",
      required: true
    ],
    filter: [
      type: :keyword_list,
      doc: "A filter to apply to the aggregate",
      default: []
    ],
    description: [
      type: :string,
      doc: "An optional description for the aggregate"
    ]
  ]

  @type t :: %__MODULE__{
          name: atom(),
          relationship_path: {:ok, list(atom())} | {:error, String.t()},
          filter: Keyword.t(),
          kind: :count,
          description: String.t() | nil
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
