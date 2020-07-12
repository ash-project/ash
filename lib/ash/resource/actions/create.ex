defmodule Ash.Resource.Actions.Create do
  @moduledoc "Represents a create action on a resource."
  defstruct [:name, :primary?, type: :create]

  @type t :: %__MODULE__{
          type: :create,
          name: atom,
          primary?: boolean
        }

  @opt_schema [
    name: [
      type: :atom,
      required: true,
      doc: "The name of the action"
    ],
    primary?: [
      type: :boolean,
      default: false,
      doc: "Whether or not this action should be used when no action is specified by the caller."
    ]
  ]

  @doc false
  def opt_schema, do: @opt_schema
end
