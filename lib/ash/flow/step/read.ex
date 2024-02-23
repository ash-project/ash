defmodule Ash.Flow.Step.Read do
  @moduledoc "Represents a read action step in an Ash.Flow"
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
          Whether or not read action is expected to return a single result or `nil`. Set to `true` automatically if `get? true`.
          """,
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
      |> Spark.Options.merge(@shared_opts, "Global Options")
      |> Spark.Options.merge(@shared_action_opts, "Action Step Opts")
end
