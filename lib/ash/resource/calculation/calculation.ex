defmodule Ash.Resource.Calculation do
  @moduledoc "Represents a named calculation on a resource"
  defstruct [
    :name,
    :type,
    :calculation,
    :arguments,
    :description,
    :constraints,
    :private?,
    :allow_nil?,
    :select,
    :load,
    :allow_async?,
    filterable?: true
  ]

  @schema [
    name: [
      type: :atom,
      required: true,
      doc: "The field name to use for the calculation value"
    ],
    type: [
      type: :any,
      required: true
    ],
    constraints: [
      type: :keyword_list,
      default: [],
      doc: "Constraints to provide to the type."
    ],
    allow_async?: [
      type: :boolean,
      default: false,
      doc: """
      If set to `true`, then the calculation may be run after the main query.

      This is useful for calculations that are very expensive, especially when combined with complex filters/join
      scenarios. By adding this, we will rerun a trimmed down version of the main query, using the primary keys for
      fast access. This will be done asynchronously for each calculation that has `allow_async?: true`.

      Keep in mind that if the calculation is used in a filter or sort, it cannot be done asynchrnously,
      and *must* be done in the main query.
      """
    ],
    calculation: [
      type: {:custom, __MODULE__, :calculation, []},
      required: true,
      doc: "The module or `{module, opts}` to use for the calculation"
    ],
    description: [
      type: :string,
      doc: "An optional description for the calculation"
    ],
    private?: [
      type: :boolean,
      default: false,
      doc:
        "Whether or not the calculation will appear in any interfaces created off of this resource, e.g AshJsonApi and AshGraphql"
    ],
    select: [
      type: {:list, :atom},
      default: [],
      doc: "A list of fields to ensure selected in the case that the calculation is run."
    ],
    load: [
      type: :any,
      default: [],
      doc: "A load statement to be applied if the calculation is used."
    ],
    allow_nil?: [
      type: :boolean,
      default: true,
      doc: "Whether or not the calculation can return nil."
    ],
    filterable?: [
      type: {:or, [:boolean, {:in, [:simple_equality]}]},
      default: true,
      doc: "Whether or not the calculation should be usable in filters."
    ]
  ]

  @type t :: %__MODULE__{
          name: atom(),
          calculation: {:ok, {atom(), any()}} | {:error, String.t()},
          arguments: list(any()),
          description: String.t() | nil,
          private?: boolean,
          allow_nil?: boolean
        }

  defmodule Argument do
    @moduledoc "An argument to a calculation"
    defstruct [:name, :type, :default, :allow_nil?, :constraints]

    @schema [
      name: [
        type: :atom,
        required: true,
        doc: "The name to use for the argument"
      ],
      type: [
        type: :ash_type,
        required: true,
        doc: "The type of the argument"
      ],
      default: [
        type: {:or, [{:mfa_or_fun, 0}, :literal]},
        required: false,
        doc: "A default value to use for the argument if not provided"
      ],
      allow_nil?: [
        type: :boolean,
        default: true,
        doc: "Whether or not the argument value may be nil"
      ],
      constraints: [
        type: :keyword_list,
        default: [],
        doc:
          "Constraints to provide to the type when casting the value. See the type's documentation for more information."
      ]
    ]

    def schema, do: @schema
  end

  def schema, do: @schema

  def calculation({module, opts}) when is_atom(module) and is_list(opts),
    do: {:ok, {module, opts}}

  def calculation(module) when is_atom(module), do: {:ok, {module, []}}

  def calculation(other) do
    {:ok, {Ash.Resource.Calculation.Expression, expr: other}}
  end
end
