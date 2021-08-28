defmodule Ash.Resource.Actions.Metadata do
  @moduledoc "Represents metadata from an action"
  defstruct [
    :allow_nil?,
    :type,
    :name,
    :default,
    :description,
    constraints: []
  ]

  @type t :: %__MODULE__{}

  def schema do
    [
      allow_nil?: [
        type: :boolean,
        default: true,
        doc: "Whether or not the metadata may return `nil`"
      ],
      type: [
        type: :any,
        required: true,
        doc: "The type of the metadata"
      ],
      name: [
        type: :atom,
        required: true,
        doc: "The name of the metadata"
      ],
      default: [
        type: :any,
        doc:
          "The default value for the metadata to take. It can be a zero argument function e.g `&MyMod.my_fun/0` or a value"
      ],
      constraints: [
        type: :keyword_list,
        default: [],
        doc: "Type constraints on the metadata"
      ],
      description: [
        type: :string,
        doc: "An optional description for the metadata."
      ]
    ]
  end
end
