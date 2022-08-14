defmodule Ash.Resource.Actions.Argument do
  @moduledoc "Represents an argument to an action"
  defstruct [
    :allow_nil?,
    :type,
    :name,
    :default,
    :private?,
    :sensitive?,
    :description,
    constraints: []
  ]

  @type t :: %__MODULE__{}

  def schema do
    [
      allow_nil?: [
        type: :boolean,
        default: true,
        doc: "Whether or not the argument may be ommitted or set to `nil`"
      ],
      type: [
        type: Ash.OptionsHelpers.ash_type(),
        required: true,
        doc: "The type of the argument"
      ],
      name: [
        type: :atom,
        required: true,
        doc: "The name of the argument"
      ],
      private?: [
        type: :boolean,
        default: false,
        doc: "Whether or not the argument should be part of the public API"
      ],
      sensitive?: [
        type: :boolean,
        default: false,
        doc: "Whether or not the attribute value contains sensitive information, like PII.",
        links: [
          guides: ["ash:guide:Security"]
        ]
      ],
      default: [
        type: :any,
        doc:
          "The default value for the argument to take. It can be a zero argument function e.g `&MyMod.my_fun/0` or a value"
      ],
      constraints: [
        type: :keyword_list,
        default: [],
        doc: "Type constraints on the argument"
      ],
      description: [
        type: :string,
        doc: "An optional description for the argument."
      ]
    ]
  end
end
