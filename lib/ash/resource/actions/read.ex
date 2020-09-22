defmodule Ash.Resource.Actions.Read do
  @moduledoc "Represents a read action on a resource."

  defstruct [:name, :primary?, :filter, :description, type: :read]

  @type t :: %__MODULE__{
          type: :read,
          name: atom,
          primary?: boolean,
          description: String.t()
        }

  import Ash.Resource.Actions.SharedOptions

  @global_opts shared_options()

  @opt_schema Ash.OptionsHelpers.merge_schemas(
                [
                  filter: [
                    type: :any,
                    doc:
                      "A filter template, that may contain actor references. See `Ash.Filter` for more on templates"
                  ]
                ],
                @global_opts,
                "Action Options"
              )

  @doc false
  def opt_schema, do: @opt_schema
end
