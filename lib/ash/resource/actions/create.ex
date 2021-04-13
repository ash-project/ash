defmodule Ash.Resource.Actions.Create do
  @moduledoc "Represents a create action on a resource."
  defstruct [
    :name,
    :primary?,
    :description,
    accept: nil,
    arguments: [],
    changes: [],
    allow_nil_input: [],
    reject: [],
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
                  A list of attributes that would normally be required, but should not be for this action.

                  This exists because extensions like ash_graphql and ash_json_api will add non-null validations to their input for any attribute
                  that is accepted by an action that has `allow_nil?: false`. This tells those extensions that some `change` on the resource might
                  set that attribute, and so they should not require it at the API layer.

                  Ash core doesn't actually use the values in this list, because it does its `nil` validation *after* running all resource
                  changes. If the value is still `nil` by the time Ash would submit to the data layer, then an error is returned.
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
