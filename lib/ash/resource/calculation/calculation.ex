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
      links: [],
      doc: "The field name to use for the calculation value"
    ],
    type: [
      type: :any,
      links: [
        modules: [
          "ash:module:Ash.Type"
        ]
      ],
      doc: "The type of the calculation",
      required: true
    ],
    constraints: [
      type: :keyword_list,
      default: [],
      links: [
        modules: [
          "ash:module:Ash.Type"
        ]
      ],
      doc: "Constraints to provide to the type."
    ],
    allow_async?: [
      type: :boolean,
      default: false,
      links: [
        guides: [
          "ash:guide:Actions"
        ]
      ],
      doc: """
      If set to `true`, then the calculation may be run after the main query.
      """
    ],
    calculation: [
      type: {:custom, __MODULE__, :calculation, []},
      required: true,
      links: [],
      doc: "The module or `{module, opts}` to use for the calculation"
    ],
    description: [
      type: :string,
      links: [],
      doc: "An optional description for the calculation"
    ],
    private?: [
      type: :boolean,
      default: false,
      links: [
        guides: [
          "ash:guide:Security"
        ]
      ],
      doc:
        "Whether or not the calculation will appear in any interfaces created off of this resource, e.g AshJsonApi and AshGraphql"
    ],
    select: [
      type: {:list, :atom},
      default: [],
      links: [],
      doc: "A list of fields to ensure selected if the calculation is used."
    ],
    load: [
      type: :any,
      default: [],
      links: [],
      doc: "A load statement to be applied if the calculation is used."
    ],
    allow_nil?: [
      type: :boolean,
      default: true,
      links: [],
      doc: "Whether or not the calculation can return nil."
    ],
    filterable?: [
      type: {:or, [:boolean, {:in, [:simple_equality]}]},
      default: true,
      links: [],
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

  @type ref :: {module(), Keyword.t()} | module()

  defmodule Argument do
    @moduledoc "An argument to a calculation"
    defstruct [:name, :type, :default, :allow_nil?, :constraints, :allow_expr?]

    @schema [
      name: [
        type: :atom,
        required: true,
        doc: "The name of the argument",
        links: []
      ],
      type: [
        type: Ash.OptionsHelpers.ash_type(),
        required: true,
        links: [
          modules: [
            "ash:module:Ash.Type"
          ]
        ],
        doc: "The type of the argument"
      ],
      default: [
        type: {:or, [{:mfa_or_fun, 0}, :literal]},
        required: false,
        links: [],
        doc: "A default value to use for the argument if not provided"
      ],
      allow_nil?: [
        type: :boolean,
        default: true,
        links: [],
        doc: "Whether or not the argument value may be nil (or may be not provided)"
      ],
      allow_expr?: [
        type: :boolean,
        default: false,
        links: [],
        doc:
          "Experimental option to allow passing expressions as argument values. Expressions cannot be type validated."
      ],
      constraints: [
        type: :keyword_list,
        default: [],
        links: [
          modules: [
            "ash:module:Ash.Type"
          ]
        ],
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
