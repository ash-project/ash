defmodule Ash.Resource.Actions.Update do
  @moduledoc "Represents a update action on a resource."

  require Ash.Flags

  defstruct [
    :name,
    :primary?,
    :description,
    :error_handler,
    accept: nil,
    manual: nil,
    manual?: false,
    require_atomic?: false,
    notifiers: [],
    atomics: [],
    require_attributes: [],
    delay_global_validations?: false,
    skip_global_validations?: false,
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
          notifiers: list(module),
          accept: list(atom),
          require_atomic?: boolean,
          arguments: list(Ash.Resource.Actions.Argument.t()),
          delay_global_validations?: boolean,
          skip_global_validations?: boolean,
          primary?: boolean,
          touches_resources: list(atom),
          description: String.t() | nil
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
                  Override the update behavior. Accepts a module or module and opts, or a function that takes the changeset and context. See the [manual actions guide](/documentation/topics/manual-actions.md) for more.
                  """
                ],
                require_atomic?: [
                  type: :boolean,
                  doc: """
                  Require that the update be atomic. This means that all changes and validations implement the `atomic` callback. See the guide on atomic updates for more.
                  """,
                  default: true
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
