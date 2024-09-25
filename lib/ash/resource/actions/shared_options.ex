defmodule Ash.Resource.Actions.SharedOptions do
  @moduledoc false

  @shared_options [
    name: [
      type: :atom,
      required: true,
      doc: "The name of the action"
    ],
    primary?: [
      type: :boolean,
      default: false,
      doc: "Whether or not this action should be used when no action is specified by the caller."
    ],
    description: [
      type: :string,
      doc: "An optional description for the action"
    ],
    transaction?: [
      type: :boolean,
      doc: """
      Whether or not the action should be run in transactions. Reads default to false, while create/update/destroy actions default to `true`.
      """
    ],
    touches_resources: [
      type: {:list, :atom},
      doc: """
      A list of resources that the action may touch, used when building transactions.
      """
    ],
    skip_unknown_inputs: [
      type: {:wrap_list, {:or, [:atom, :string]}},
      default: [],
      doc: "A list of unknown fields to skip, or `:*` to skip all unknown fields."
    ]
  ]

  @create_update_opts [
    accept: [
      type: {:or, [{:wrap_list, :atom}, {:literal, :*}]},
      doc: "The list of attributes to accept. Use `:*` to accept all public attributes."
    ],
    action_select: [
      type: {:list, :atom},
      doc: """
      A list of attributes that the action requires to do its work. Defaults to all attributes except those with `select_by_default? false`. On actions with no changes/notifiers, it defaults to the externally selected attributes. Keep in mind that action_select is applied *before* notifiers.
      """
    ],
    require_attributes: [
      type: {:list, :atom},
      doc: """
      A list of attributes that would normally `allow_nil?`, to require for this action. No need to include attributes that already do not allow nil?
      """
    ],
    allow_nil_input: [
      type: {:list, :atom},
      doc: """
      A list of attributes that would normally be required, but should not be for this action. They will still be validated just before the data layer step.
      """
    ],
    delay_global_validations?: [
      type: :boolean,
      default: false,
      doc: """
      If true, global validations will be done in a `before_action` hook, regardless of their configuration on the resource.
      """
    ],
    skip_global_validations?: [
      type: :boolean,
      default: false,
      doc: """
      If true, global validations will be skipped. Useful for manual actions.
      """
    ],
    error_handler: [
      type: :mfa,
      doc: "Sets the error handler on the changeset. See `Ash.Changeset.handle_errors/2` for more"
    ],
    notifiers: [
      type: {:list, {:behaviour, Ash.Notifier}},
      doc: "Notifiers that will be called specifically for this action."
    ],
    manual?: [
      type: :boolean,
      doc: """
      Instructs Ash to *skip* the actual update/create/destroy step at the data layer. See the [manual actions guide](/documentation/topics/manual-actions.md) for more.
      """
    ]
  ]

  def shared_options do
    @shared_options
  end

  def create_update_opts do
    @create_update_opts
  end
end
