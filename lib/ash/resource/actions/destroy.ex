defmodule Ash.Resource.Actions.Destroy do
  @moduledoc "Represents a destroy action on a resource."

  defstruct [
    :name,
    :primary?,
    :soft?,
    :description,
    arguments: [],
    accept: nil,
    changes: [],
    reject: [],
    require_attributes: [],
    type: :destroy
  ]

  @type t :: %__MODULE__{
          type: :destroy,
          name: atom,
          arguments: [Ash.Resource.Actions.Argument.t()],
          primary?: boolean,
          description: String.t()
        }
  import Ash.Resource.Actions.SharedOptions

  @global_opts shared_options()
  @create_update_opts create_update_opts()

  @opt_schema [
                soft?: [
                  type: :atom,
                  doc:
                    "If specified, the destroy action calls the datalayer's update function with any specified changes."
                ]
              ]
              |> Ash.OptionsHelpers.merge_schemas(
                @global_opts,
                "Action Options"
              )
              |> Ash.OptionsHelpers.merge_schemas(
                @create_update_opts,
                "Create/Update Options (for soft destroys)"
              )

  @doc false
  def opt_schema, do: @opt_schema
end
