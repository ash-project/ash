defmodule Ash.Resource.Aggregate do
  @moduledoc "Represents a named aggregate on the resource that can be loaded"
  defstruct [
    :name,
    :relationship_path,
    :filter,
    :kind,
    :implementation,
    :constraints,
    :type,
    :description,
    :private?,
    :field,
    :sort,
    :default,
    filterable?: true
  ]

  @schema [
    name: [
      type: :atom,
      doc: "The field to place the aggregate in",
      required: true,
      links: []
    ],
    relationship_path: [
      type: {:custom, __MODULE__, :relationship_path, []},
      doc: "The relationship or relationship path to use for the aggregate",
      required: true,
      links: []
    ],
    kind: [
      type:
        {:or,
         [
           {:in, [:count, :first, :sum, :list, :avg, :max, :min, :custom]},
           {:tuple, [{:in, [:custom]}, Ash.OptionsHelpers.ash_type()]}
         ]},
      doc: "The kind of the aggregate",
      required: true,
      links: []
    ],
    field: [
      type: :atom,
      doc:
        "The field to aggregate. Defaults to the first field in the primary key of the resource",
      required: false,
      links: []
    ],
    filter: [
      type: :any,
      doc: "A filter to apply to the aggregate",
      default: [],
      links: []
    ],
    sort: [
      type: :any,
      doc: "A sort to be applied to the aggregate",
      links: []
    ],
    description: [
      type: :string,
      doc: "An optional description for the aggregate",
      links: []
    ],
    default: [
      type: :any,
      doc:
        "A default value to use in cases where nil would be used. Count defaults to `0` but `first` and `count` do not have defaults.",
      links: []
    ],
    private?: [
      type: :boolean,
      default: false,
      doc:
        "Whether or not the aggregate will appear in any interfaces created off of this resource, e.g AshJsonApi and AshGraphql",
      links: []
    ],
    filterable?: [
      type: {:or, [:boolean, {:in, [:simple_equality]}]},
      default: true,
      doc: "Whether or not the aggregate should be usable in filters.",
      links: []
    ]
  ]

  @type t :: %__MODULE__{
          name: atom(),
          relationship_path: list(atom()),
          filter: Keyword.t(),
          field: atom,
          kind: Ash.Query.Aggregate.kind(),
          description: String.t() | nil,
          private?: boolean,
          default: term
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
