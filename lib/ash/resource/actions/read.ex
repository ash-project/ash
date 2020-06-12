defmodule Ash.Resource.Actions.Read do
  @moduledoc false

  defstruct [:name, :primary?, type: :read]

  @type t :: %__MODULE__{
          type: :read,
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
    ]
  ]

  @doc false
  def opt_schema, do: @opt_schema
end
