defmodule Ash.Flow.Step.Read do
  @moduledoc false
  use Ash.Flow.Step.BuiltinStep, [
    :resource,
    :action,
    :api,
    :input,
    :tenant,
    get?: false,
    not_found_error?: false
  ]

  @shared_opts Ash.Flow.Step.shared_opts()
  @shared_action_opts Ash.Flow.Step.shared_action_opts()

  def schema,
    do:
      [
        get?: [
          type: :boolean,
          doc: """
          Whether or not read action is expected to return a single result or `nil`.
          If the action is configured with `get? true` then this is automatically set to `true`.
          """,
          links: [],
          default: false
        ],
        not_found_error?: [
          type: :boolean,
          default: true,
          doc: """
          Whether or not finding no record should result in a not found error
          """
        ]
      ]
      |> Spark.OptionsHelpers.merge_schemas(@shared_opts, "Global Options")
      |> Spark.OptionsHelpers.merge_schemas(@shared_action_opts, "Action Step Opts")
end
