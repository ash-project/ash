defmodule Ash.Resource.Actions.Create do
  @moduledoc "Represents a create action on a resource."
  defstruct [
    :name,
    :primary?,
    :description,
    :error_handler,
    accept: nil,
    require_attributes: [],
    allow_nil_input: [],
    manual: nil,
    notifiers: [],
    touches_resources: [],
    action_select: nil,
    delay_global_validations?: false,
    skip_global_validations?: false,
    skip_unknown_inputs: [],
    upsert?: false,
    upsert_identity: nil,
    upsert_fields: nil,
    upsert_condition: nil,
    arguments: [],
    changes: [],
    reject: [],
    metadata: [],
    transaction?: true,
    type: :create
  ]

  @type t :: %__MODULE__{
          type: :create,
          name: atom,
          accept: nil | list(atom),
          require_attributes: list(atom),
          allow_nil_input: list(atom),
          action_select: list(atom) | nil,
          manual: module | nil,
          upsert?: boolean,
          skip_unknown_inputs: list(atom | String.t()),
          notifiers: [module()],
          delay_global_validations?: boolean,
          skip_global_validations?: boolean,
          upsert_identity: atom | nil,
          upsert_fields:
            nil
            | list(atom)
            | :replace_all
            | {:replace, list(atom)}
            | {:replace_all_except, list(atom)},
          upsert_condition: Ash.Expr.t() | nil,
          touches_resources: list(atom),
          arguments: list(Ash.Resource.Actions.Argument.t()),
          primary?: boolean,
          description: String.t() | nil
        }

  import Ash.Resource.Actions.SharedOptions

  @global_opts shared_options()
  @create_update_opts create_update_opts()

  @opt_schema [
                manual: [
                  type:
                    {:spark_function_behaviour, Ash.Resource.ManualCreate,
                     {Ash.Resource.ManualCreate.Function, 2}},
                  doc: """
                  Override the creation behavior. Accepts a module or module and opts, or a function that takes the changeset and context. See the [manual actions guide](/documentation/topics/manual-actions.md) for more.
                  """
                ],
                upsert?: [
                  type: :boolean,
                  default: false,
                  doc: """
                  Forces all uses of this action to be treated as an upsert.
                  """
                ],
                upsert_identity: [
                  type: :atom,
                  doc: """
                  The identity to use for the upsert. Cannot be overriden by the caller. Ignored  if `upsert?` is not set to `true`.
                  """
                ],
                upsert_fields: [
                  type:
                    {:or,
                     [
                       {:literal, :replace_all},
                       {:tuple, [{:literal, :replace}, {:wrap_list, :atom}]},
                       {:tuple, [{:literal, :replace_all_except}, {:wrap_list, :atom}]},
                       {:wrap_list, :atom}
                     ]},
                  doc: """
                  The fields to overwrite in the case of an upsert. If not provided, all fields except for fields set by defaults will be overwritten.
                  """
                ],
                upsert_condition: [
                  type: :any,
                  doc:
                    "An expression to check if the record should be updated when there's a conflict."
                ]
              ]
              |> Spark.Options.merge(
                @global_opts,
                "Action Options"
              )
              |> Spark.Options.merge(
                @create_update_opts,
                "Create/Update Options"
              )

  @doc false
  def opt_schema, do: @opt_schema
end
