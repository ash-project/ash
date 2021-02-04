defmodule Ash.Resource.Actions.Create do
  @moduledoc "Represents a create action on a resource."
  defstruct [
    :name,
    :primary?,
    :description,
    accept: nil,
    managed_relationships: [],
    arguments: [],
    changes: [],
    reject: [],
    type: :create
  ]

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
  @create_update_opts create_update_opts()

  @opt_schema []
              |> Ash.OptionsHelpers.merge_schemas(
                @global_opts,
                "Action Options"
              )
              |> Ash.OptionsHelpers.merge_schemas(
                @create_update_opts,
                "Create/Update Options"
              )

  @doc false
  def opt_schema, do: @opt_schema
end
