defmodule Ash.Flow.Step.Read do
  @moduledoc false
  use Ash.Flow.Step.BuiltinStep, [:resource, :action, :api, :input, :tenant, get?: false]
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
        ]
      ]
      |> Spark.OptionsHelpers.merge_schemas(@shared_opts, "Global Options")
      |> Spark.OptionsHelpers.merge_schemas(@shared_action_opts, "Action Step Opts")
end
