defmodule Ash.Resource.Calculation do
  @moduledoc "Represents a named calculation on a resource"
  defstruct [:name, :type, :calculation, :arguments, :description, :private?]

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
    ]
  ]

  @type t :: %__MODULE__{
          name: atom(),
          calculation: {:ok, {atom(), any()}} | {:error, String.t()},
          arguments: list(any()),
          description: String.t() | nil,
          private?: boolean
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
        type: :any,
        required: true,
        doc: "The type of the argument"
      ],
      default: [
        type: {:custom, Ash.OptionsHelpers, :default, []},
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
    {:error, "Expected a module or {module, opts}, got: #{inspect(other)}"}
  end
end
