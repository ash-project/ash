defmodule Ash.Resource.Actions.Destroy do
  @moduledoc "Represents a destroy action on a resource."

  require Ash.Flags

  defstruct [
    :name,
    :primary?,
    :soft?,
    :description,
    :error_handler,
    manual: nil,
    require_atomic?: false,
    arguments: [],
    touches_resources: [],
    delay_global_validations?: false,
    skip_global_validations?: false,
    accept: nil,
    changes: [],
    reject: [],
    transaction?: true,
    require_attributes: [],
    metadata: [],
    type: :destroy
  ]

  @type t :: %__MODULE__{
          type: :destroy,
          name: atom,
          manual: module | nil,
          arguments: list(Ash.Resource.Actions.Argument.t()),
          require_atomic?: boolean,
          delay_global_validations?: boolean,
          skip_global_validations?: boolean,
          touches_resources: list(atom),
          primary?: boolean,
          description: String.t() | nil
        }
  import Ash.Resource.Actions.SharedOptions

  @global_opts shared_options()
  @create_update_opts create_update_opts()

  @opt_schema [
                soft?: [
                  type: :boolean,
                  doc: "If specified, the destroy action behaves as an update internally",
                  default: false
                ],
                manual: [
                  type:
                    {:spark_function_behaviour, Ash.Resource.ManualDestroy,
                     {Ash.Resource.ManualDestroy.Function, 2}},
                  doc: """
                  Override the update behavior. Accepts a module or module and opts, or a function that takes the changeset and context. See the [manual actions guide](/documentation/topics/manual-actions.md) for more.
                  """
                ],
                require_atomic?: [
                  type: :boolean,
                  doc: """
                  Require that the update be atomic. Only relevant if `soft?` is set to `true`. This means that all changes and validations implement the `atomic` callback. See the guide on atomic updates for more.
                  """,
                  default: false
                ]
              ]
              |> Spark.OptionsHelpers.merge_schemas(
                @global_opts,
                "Action Options"
              )
              |> Spark.OptionsHelpers.merge_schemas(
                @create_update_opts,
                "Create/Update Options (for soft destroys)"
              )

  @doc false
  def opt_schema, do: @opt_schema
end
