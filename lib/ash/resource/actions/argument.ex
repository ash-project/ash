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
      description: [
        type: :string,
        doc: "An optional description for the argument."
      ],
      constraints: [
        type: :keyword_list,
        default: [],
        doc: "Type constraints on the argument"
      ],
      allow_nil?: [
        type: :boolean,
        default: true,
        doc: "Whether or not the argument value may be nil (or may be not provided)"
      ],
      private?: [
        type: :boolean,
        default: false,
        doc: "Whether or not the argument should be suppliable by the client."
      ],
      sensitive?: [
        type: :boolean,
        default: false,
        doc: """
        Whether or not the attribute value contains sensitive information, like PII.

        See the [security guide](/documentation/topics/security.md) for more.
        """
      ],
      default: [
        type: :any,
        doc:
          "The default value for the argument to take. It can be a zero argument function e.g `&MyMod.my_fun/0` or a value"
      ]
    ]
  end
end
