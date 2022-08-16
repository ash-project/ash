defmodule Ash.Flow.Argument do
  @moduledoc "An argument to a flow"
  defstruct [:name, :type, :default, :allow_nil?, :constraints]

  @schema [
    name: [
      type: :atom,
      required: true,
      doc: "The name to use for the argument",
      links: []
    ],
    type: [
      type: Ash.OptionsHelpers.ash_type(),
      required: true,
      doc: "The type of the argument",
      links: [
        modules: [
          "ash:module:Ash.Type"
        ]
      ]
    ],
    default: [
      type: {:or, [{:mfa_or_fun, 0}, :literal]},
      required: false,
      doc: "A default value to use for the argument if not provided",
      links: []
    ],
    allow_nil?: [
      type: :boolean,
      default: true,
      doc: "Whether or not the argument value may be nil",
      links: []
    ],
    constraints: [
      type: :keyword_list,
      default: [],
      doc:
        "Constraints to provide to the type when casting the value. See the type's documentation for more information.",
      links: []
    ]
  ]

  def schema, do: @schema
end
