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
    touches_resources: [
      type: {:list, :atom},
      doc: """
      A list of resources that the action may touch.

      If your action has custom code (i.e custom changes) that touch resources that use a different data layer,
      this can be used to inform the transaction logic that these resource's data layer's should be involved in the
      transaction. In most standard set ups, this should not be necessary.
      """
    ]
  ]

  @create_update_opts [
    accept: [
      type: {:custom, Ash.OptionsHelpers, :list_of_atoms, []},
      doc: "The list of attributes to accept. Defaults to all attributes on the resource"
    ],
    reject: [
      type: {:custom, Ash.OptionsHelpers, :list_of_atoms, []},
      doc: """
      A list of attributes not to accept. This is useful if you want to say 'accept all but x'

      If this is specified along with `accept`, then everything in the `accept` list minus any matches in the
      `reject` list will be accepted.
      """
    ],
    require_attributes: [
      type: {:custom, Ash.OptionsHelpers, :list_of_atoms, []},
      doc: """
      A list of attributes that would normally `allow_nil` to require for this action.

      No need to include attributes that are `allow_nil?: false`.
      """
    ],
    error_handler: [
      type: :mfa,
      doc: "Sets the error handler on the changeset. See `Ash.Changeset.handle_errors/2` for more"
    ],
    manual?: [
      type: :boolean,
      doc: """
      Instructs Ash to *skip* the actual update/create/destroy step.

      All validation still takes place, but the `result` in any `after_action` callbacks
      attached to that action will simply be the record that was read from the database initially.
      For creates, the `result` will be `nil`, and you will be expected to handle the changeset in
      an after_action callback and return an instance of the record. This is a good way to prevent
      Ash from issuing an unnecessary update to the record, e.g updating the `updated_at` of the record
      when an action actually only involves modifying relating records.

      You could then handle the changeset automatically.

      For example:

      # in the action

      ```elixir
      action :special_create do
        manual? true
        change MyApp.DoCreate
      end

      # The change
      defmodule MyApp.DoCreate do
        use Ash.Resource.Change

        def change(changeset, _, _) do
          Ash.Changeset.after_action(changeset, fn changeset, _result ->
            # result will be `nil`, because this is a manual action

            result = do_something_that_creates_the_record(changeset)

            {:ok, result}
          end)
        end
      end
      ```
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
