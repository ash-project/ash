defmodule Ash.Resource.Actions.Create do
  @moduledoc "Represents a create action on a resource."
  defstruct [
    :name,
    :primary?,
    :description,
    :error_handler,
    accept: nil,
    manual?: false,
    manual: nil,
    touches_resources: [],
    require_attributes: [],
    delay_global_validations?: false,
    skip_global_validations?: false,
    upsert?: false,
    upsert_identity: nil,
    arguments: [],
    changes: [],
    allow_nil_input: [],
    reject: [],
    metadata: [],
    transaction?: true,
    type: :create
  ]

  @type t :: %__MODULE__{
          type: :create,
          name: atom,
          accept: list(atom),
          manual: module | nil,
          upsert?: boolean,
          delay_global_validations?: boolean,
          skip_global_validations?: boolean,
          upsert_identity: atom | nil,
          allow_nil_input: list(atom),
          touches_resources: list(atom),
          arguments: list(Ash.Resource.Actions.Argument.t()),
          primary?: boolean,
          description: String.t() | nil
        }

  import Ash.Resource.Actions.SharedOptions

  @global_opts shared_options()
  @create_update_opts create_update_opts()

  @opt_schema [
                allow_nil_input: [
                  type: {:list, :atom},
                  doc: """
                  A list of attributes that would normally be required, but should not be for this action. They will still be validated just before
                  the record is created.
                  """
                ],
                manual: [
                  type:
                    {:spark_function_behaviour, Ash.Resource.ManualCreate,
                     {Ash.Resource.ManualCreate.Function, 2}},
                  doc: """
                  Override the creation behavior. See the manual action guides for more. Accepts a module or module and opts, or a function that takes the changeset and context.

                  See the [manual actions guide](/documentation/topics/manual-actions.md) for more.
                  """
                ],
                upsert?: [
                  type: :boolean,
                  default: false,
                  doc: """
                  Wether or not this action is always an upsert.

                  If this is false, the action can still be used as an upsert by passing `upsert?: true` when using it.
                  This option forces all uses of this action to be treated as an upsert
                  """
                ],
                upsert_identity: [
                  type: :atom,
                  doc: """
                  The identity to use for the upsert. Cannot be overriden by the caller. Ignored  if `upsert?` is not set to `true`.
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
