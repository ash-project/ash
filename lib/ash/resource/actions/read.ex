defmodule Ash.Resource.Actions.Read do
  @moduledoc "Represents a read action on a resource."

  defstruct [:name, :primary?, :filter, :description, type: :read]

  @type t :: %__MODULE__{
          type: :read,
          name: atom,
          primary?: boolean,
          description: String.t()
        }

  @opt_schema [
    name: [
      type: :atom,
      doc: "The name of the action"
    ],
    filter: [
      type: :any,
      doc:
        "A filter template, that may contain actor references. See `Ash.Filter` for more on templates"
    ],
    primary?: [
      type: :boolean,
      default: false,
      doc: "Whether or not this action should be used when no action is specified by the caller."
    ],
    description: [
      type: :string,
      doc: "An optional description for the read action"
    ]
  ]

  @doc false
  def opt_schema, do: @opt_schema
end
