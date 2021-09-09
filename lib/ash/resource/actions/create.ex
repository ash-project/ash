defmodule Ash.Resource.Actions.Create do
  @moduledoc "Represents a create action on a resource."
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
    allow_nil_input: [],
    reject: [],
    metadata: [],
    type: :create
  ]

  @type t :: %__MODULE__{
          type: :create,
          name: atom,
          accept: [atom],
          allow_nil_input: [atom],
          arguments: [Ash.Resource.Actions.Argument.t()],
          primary?: boolean,
          description: String.t()
        }

  import Ash.Resource.Actions.SharedOptions

  @global_opts shared_options()
  @create_update_opts create_update_opts()

  @opt_schema [
                allow_nil_input: [
                  type: {:list, :atom},
                  doc: """
                  A list of attributes that would normally be required, but should not be for this action. They will still be validated just before
                  the record is created, but this allows for setting required attributes in your changes under the hood.
                  """
                ]
              ]
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
