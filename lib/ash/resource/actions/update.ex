defmodule Ash.Resource.Actions.Update do
  @moduledoc "Represents a update action on a resource."

  defstruct [
    :name,
    :primary?,
    :description,
    :error_handler,
    accept: nil,
    manual?: false,
    require_attributes: [],
    arguments: [],
    changes: [],
    reject: [],
    metadata: [],
    touches_resources: [],
    type: :update
  ]

  @type t :: %__MODULE__{
          type: :update,
          name: atom,
          accept: list(atom),
          arguments: list(Ash.Resource.Actions.Argument.t()),
          primary?: boolean,
          touches_resources: list(atom),
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
