defmodule Ash.Resource.Actions.Create do
  @moduledoc "Represents a create action on a resource."
  defstruct [:name, :primary?, :accept, :arguments, :changes, :description, type: :create]

  @type t :: %__MODULE__{
          type: :create,
          name: atom,
          accept: [atom],
          arguments: [Ash.Resource.Actions.Argument.t()],
          primary?: boolean,
          description: String.t()
        }

  import Ash.Resource.Actions.SharedOptions

  @global_opts shared_options()

  @opt_schema Ash.OptionsHelpers.merge_schemas(
                [
                  accept: [
                    type: {:custom, Ash.OptionsHelpers, :list_of_atoms, []},
                    doc:
                      "The list of attributes and relationships to accept. Defaults to all attributes on the resource"
                  ]
                ],
                @global_opts,
                "Action Options"
              )

  @doc false
  def opt_schema, do: @opt_schema
end
