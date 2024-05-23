defmodule Ash.Resource.Actions.Update do
  @moduledoc "Represents a update action on a resource."

  require Ash.Flags

  defstruct [
    :name,
    :primary?,
    :description,
    :error_handler,
    accept: nil,
    require_attributes: [],
    allow_nil_input: [],
    manual: nil,
    manual?: false,
    require_atomic?: Application.compile_env(:ash, :require_atomic_by_default?, true),
    atomic_upgrade?: true,
    atomic_upgrade_with: nil,
    notifiers: [],
    atomics: [],
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
          atomic_upgrade?: boolean(),
          atomic_upgrade_with: nil | atom(),
          notifiers: list(module),
          accept: list(atom),
          require_attributes: list(atom),
          allow_nil_input: list(atom),
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
                  default: Application.compile_env(:ash, :require_atomic_by_default?, true)
                ],
                atomic_upgrade?: [
                  type: :boolean,
                  doc: """
                  If set to `true`, atomic upgrades will be performed. Ignored if `required_atomic?` is `true`. See the update actions guide for more.
                  """,
                  default: false
                ],
                atomic_upgrade_with: [
                  type: {:one_of, [:atom, nil]},
                  doc: """
                  Configure the read action used when performing atomic upgrades. Defaults to the primary read action.
                  """
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

  def transform(%{manual: manual} = action) when not is_nil(manual) do
    {:ok, %{action | require_atomic?: false}}
  end

  def transform(action), do: {:ok, action}
end
