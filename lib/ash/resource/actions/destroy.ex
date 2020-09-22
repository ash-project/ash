defmodule Ash.Resource.Actions.Destroy do
  @moduledoc "Represents a destroy action on a resource."

  defstruct [:name, :primary?, :changes, :accept, :soft?, :description, type: :destroy]

  @type t :: %__MODULE__{
          type: :destroy,
          name: atom,
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
                      "The list of attributes and relationships to accept. Defaults to all attributes on the resource. Has no effect unless `soft?` is specified."
                  ],
                  soft?: [
                    type: :atom,
                    doc:
                      "If specified, the destroy action calls the datalayer's update function with any specified changes."
                  ]
                ],
                @global_opts,
                "Action Options"
              )

  @doc false
  def opt_schema, do: @opt_schema
end
