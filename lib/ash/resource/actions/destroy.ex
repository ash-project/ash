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
    require_atomic?: Application.compile_env(:ash, :require_atomic_by_default?, true),
    skip_unknown_inputs: [],
    atomic_upgrade?: true,
    atomic_upgrade_with: nil,
    action_select: nil,
    arguments: [],
    touches_resources: [],
    delay_global_validations?: false,
    skip_global_validations?: false,
    notifiers: [],
    accept: nil,
    require_attributes: [],
    allow_nil_input: [],
    changes: [],
    reject: [],
    transaction?: true,
    metadata: [],
    type: :destroy
  ]

  @type t :: %__MODULE__{
          type: :destroy,
          name: atom,
          manual: module | nil,
          action_select: list(atom) | nil,
          notifiers: list(module),
          arguments: list(Ash.Resource.Actions.Argument.t()),
          atomic_upgrade?: boolean(),
          skip_unknown_inputs: list(atom() | String.t()),
          atomic_upgrade_with: atom() | nil,
          require_atomic?: boolean,
          accept: nil | list(atom),
          require_attributes: list(atom),
          allow_nil_input: list(atom),
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
                  default: Application.compile_env(:ash, :require_atomic_by_default?, true)
                ],
                atomic_upgrade?: [
                  type: :boolean,
                  doc: """
                  If set to `true`, atomic upgrades will be performed. See the update actions guide for more.
                  """,
                  default: false
                ],
                atomic_upgrade_with: [
                  type: {:or, [:atom, nil]},
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
                "Create/Update Options (for soft destroys)"
              )

  @doc false
  def opt_schema, do: @opt_schema

  def transform(%{manual: manual} = action) when not is_nil(manual) do
    {:ok, %{action | require_atomic?: false}}
  end

  def transform(action), do: {:ok, action}
end
