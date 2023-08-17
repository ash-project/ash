defmodule Ash.Resource.Calculation.Argument do
  @moduledoc "An argument to a calculation"
  defstruct allow_nil?: true,
            allow_expr?: false,
            constraints: [],
            default: nil,
            name: nil,
            type: nil

  @type t :: %__MODULE__{
          allow_nil?: boolean,
          allow_expr?: boolean,
          constraints: keyword,
          default: any,
          name: atom,
          type: Ash.Type.t()
        }

  @schema [
    name: [
      type: :atom,
      required: true,
      doc: "The name of the argument"
    ],
    type: [
      type: Ash.OptionsHelpers.ash_type(),
      required: true,
      doc: "The type of the argument. See `Ash.Type` for more."
    ],
    default: [
      type: {:or, [{:mfa_or_fun, 0}, :literal]},
      required: false,
      doc: "A default value to use for the argument if not provided"
    ],
    allow_nil?: [
      type: :boolean,
      default: true,
      doc: "Whether or not the argument value may be nil (or may be not provided)"
    ],
    allow_expr?: [
      type: :boolean,
      default: false,
      doc: "Allow passing expressions as argument values. Expressions cannot be type validated."
    ],
    constraints: [
      type: :keyword_list,
      default: [],
      doc:
        "Constraints to provide to the type when casting the value. See the type's documentation and `Ash.Type` for more."
    ]
  ]

  def schema, do: @schema
end
