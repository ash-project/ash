defmodule Ash.Resource.Actions.Update do
  @moduledoc "Represents a update action on a resource."

  defstruct [
    :name,
    :primary?,
    :description,
    :error_handler,
    accept: nil,
    manual: nil,
    manual?: false,
    require_attributes: [],
    arguments: [],
    changes: [],
    reject: [],
    metadata: [],
    transaction?: true,
    touches_resources: [],
    type: :update
  ]

  @type t :: %__MODULE__{
          type: :update,
          name: atom,
          manual: module | nil,
          accept: list(atom),
          arguments: list(Ash.Resource.Actions.Argument.t()),
          primary?: boolean,
          touches_resources: list(atom),
          description: String.t()
        }

  import Ash.Resource.Actions.SharedOptions

  @global_opts shared_options()
  @create_update_opts create_update_opts()

  @opt_schema [
                manual: [
                  type:
                    {:spark_function_behaviour, Ash.Resource.ManualUpdate,
                     {Ash.Resource.ManualUpdate.Function, 2}},
                  doc: """
                  Override the update behavior. See the manual action guides for more.
                  Accepts a module or module and opts, or a function that takes the changeset and context.

                  See the [manual actions guide](/documentation/topics/manual-actions.md) for more.
                  """
                ]
              ]
              |> Spark.OptionsHelpers.merge_schemas(
                @global_opts,
                "Action Options"
              )
              |> Spark.OptionsHelpers.merge_schemas(
                @create_update_opts,
                "Create/Update Options"
              )

  @doc false
  def opt_schema, do: @opt_schema
end
