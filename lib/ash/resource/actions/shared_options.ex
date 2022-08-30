defmodule Ash.Resource.Actions.SharedOptions do
  @moduledoc false

  @shared_options [
    name: [
      type: :atom,
      required: true,
      doc: "The name of the action",
      links: []
    ],
    primary?: [
      type: :boolean,
      default: false,
      doc: "Whether or not this action should be used when no action is specified by the caller.",
      links: []
    ],
    description: [
      type: :string,
      doc: "An optional description for the action",
      links: []
    ],
    transaction?: [
      type: :boolean,
      doc: """
      Whether or not the action should be run in transactions. Reads default to false, while create/update/destroy actions default to `true`.

      Has no effect if the data layer does not support transactions, or if that data layer is already in a transaction.
      """,
      links: []
    ],
    touches_resources: [
      type: {:list, :atom},
      doc: """
      A list of resources that the action may touch, used when building transactions.
      """,
      links: []
    ]
  ]

  @create_update_opts [
    accept: [
      type: {:or, [in: [:all], list: :atom]},
      doc: "The list of attributes to accept. Defaults to all attributes on the resource",
      links: []
    ],
    reject: [
      type: {:or, [in: [:all], list: :atom]},
      doc: """
      A list of attributes not to accept. This is useful if you want to say 'accept all but x'

      If this is specified along with `accept`, then everything in the `accept` list minus any matches in the
      `reject` list will be accepted.
      """,
      links: []
    ],
    require_attributes: [
      type: {:list, :atom},
      links: [],
      doc: """
      A list of attributes that would normally `allow_nil?`, to require for this action.

      No need to include attributes that already do not allow nil?
      """
    ],
    error_handler: [
      type: :mfa,
      links: [],
      doc: "Sets the error handler on the changeset. See `Ash.Changeset.handle_errors/2` for more"
    ],
    manual?: [
      type: :boolean,
      links: [
        guides: [
          "ash:guide:Manual Actions"
        ]
      ],
      doc: """
      Instructs Ash to *skip* the actual update/create/destroy step at the data layer. See the manual action guides for more.
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
