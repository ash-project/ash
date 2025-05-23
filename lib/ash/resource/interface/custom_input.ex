defmodule Ash.Resource.Interface.CustomInput do
  @moduledoc "Represents a custom input to a code interface"
  defstruct [
    :allow_nil?,
    :type,
    :name,
    :default,
    :sensitive?,
    :description,
    :transform,
    constraints: []
  ]

  defmodule Transform do
    @moduledoc "Represents a transformation applied to a custom input"
    defstruct [:to, :using]
  end

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
        doc: """
        Constraints to provide to the type when casting the value. For more information, see `Ash.Type`.
        """
      ],
      allow_nil?: [
        type: :boolean,
        default: true,
        doc: """
        Whether or not the argument value may be nil (or may be not provided). If nil value is given error is raised.
        """
      ],
      sensitive?: [
        type: :boolean,
        default: false,
        doc: """
        Whether or not the argument value contains sensitive information, like PII(Personally Identifiable Information). See the [security guide](/documentation/topics/security/sensitive-data.md) for more.
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
