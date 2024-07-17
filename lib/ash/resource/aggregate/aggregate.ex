defmodule Ash.Resource.Aggregate do
  @moduledoc "Represents a named aggregate on the resource that can be loaded"
  defstruct [
    :name,
    :relationship_path,
    :filter,
    :kind,
    :implementation,
    :read_action,
    :constraints,
    :type,
    :description,
    :public?,
    :field,
    :sort,
    :default,
    :uniq?,
    include_nil?: false,
    join_filters: [],
    authorize?: true,
    filterable?: true,
    sortable?: true,
    sensitive?: false
  ]

  defmodule JoinFilter do
    @moduledoc "Represents a join filter on a resource aggregate"
    defstruct [:relationship_path, :filter]
  end

  @schema [
    name: [
      type: :atom,
      doc: "The field to place the aggregate in",
      required: true
    ],
    read_action: [
      type: :atom,
      doc: """
      The read action to use when building the aggregate. Defaults to the primary read action. Keep in mind this action must not have any required arguments.
      """
    ],
    relationship_path: [
      type: {:wrap_list, :atom},
      doc: "The relationship or relationship path to use for the aggregate",
      required: true
    ],
    kind: [
      type:
        {:or,
         [
           {:in, [:count, :first, :sum, :list, :avg, :max, :min, :exists, :custom]},
           {:tuple, [{:in, [:custom]}, Ash.OptionsHelpers.ash_type()]}
         ]},
      doc: "The kind of the aggregate. Pre-set when using the `Ash.Resource` DSL.",
      hide: true
    ],
    field: [
      type: :atom,
      doc:
        "The field to aggregate. Defaults to the first field in the primary key of the resource",
      required: false
    ],
    filter: [
      type: :any,
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
    default: [
      type: :any,
      doc: "A default value to use in cases where nil would be used. Count defaults to `0`."
    ],
    public?: [
      type: :boolean,
      default: false,
      doc: "Whether or not the aggregate will appear in public interfaces"
    ],
    filterable?: [
      type: {:or, [:boolean, {:in, [:simple_equality]}]},
      default: true,
      doc: "Whether or not the aggregate should be usable in filters."
    ],
    sortable?: [
      type: :boolean,
      default: true,
      doc: "Whether or not the aggregate should be usable in sorts."
    ],
    sensitive?: [
      type: :boolean,
      default: false,
      doc: "Whether or not the aggregate should be considered sensitive."
    ],
    authorize?: [
      type: :boolean,
      default: true,
      doc: """
      Whether or not the aggregate query should authorize based on the target action, if the parent query is authorized. Requires filter checks on the target action.
      """
    ]
  ]

  @type t :: %__MODULE__{
          name: atom(),
          relationship_path: list(atom()),
          filter: Keyword.t(),
          field: atom,
          kind: Ash.Query.Aggregate.kind(),
          description: String.t() | nil,
          public?: boolean,
          authorize?: boolean,
          read_action: atom | nil,
          default: term,
          join_filters: %{list(atom) => term()},
          filterable?: boolean,
          sortable?: boolean,
          sensitive?: boolean
        }

  @doc false
  def schema, do: @schema
end
