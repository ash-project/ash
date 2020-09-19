defmodule Ash.Resource.Actions.Destroy do
  @moduledoc "Represents a destroy action on a resource."

  defstruct [:name, :primary?, :changes, :accept, :soft?, type: :destroy]

  @type t :: %__MODULE__{
          type: :destroy,
          name: atom,
          primary?: boolean
        }

  @opt_schema [
    name: [
      type: :atom,
      doc: "The name of the action"
    ],
    primary?: [
      type: :boolean,
      default: false,
      doc: "Whether or not this action should be used when no action is specified by the caller."
    ],
    accept: [
      type: {:custom, Ash.OptionsHelpers, :list_of_atoms, []},
      doc:
        "The list of attributes and relationships to accept. Defaults to all attributes on the resource. Has no effect unless `soft?` is specified."
    ],
    soft?: [
      type: :atom,
      doc:
        "If specified, the destroy action calls the datalayer's update function with any specified changes."
    ]
  ]

  @doc false
  def opt_schema, do: @opt_schema
end
