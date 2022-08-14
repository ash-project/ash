defmodule Ash.Flow.Step.RunFlow do
  @moduledoc "Runs another flow"
  use Ash.Flow.Step.BuiltinStep, [:input, :flow, :built]
  @shared_opts Ash.Flow.Step.shared_opts()

  def schema,
    do:
      [
        flow: [
          type: :atom,
          doc: "The flow to run.",
          required: true
        ],
        input: Ash.Flow.Step.input(),
        get?: [
          type: :boolean,
          doc: """
          Whether or not read action is expected to return 0 or 1 results.

          Action result will be `nil` or a record.
          If the action is configured with `get? true` then this is automatically set to `true`.
          """,
          default: false
        ]
      ]
      |> Spark.OptionsHelpers.merge_schemas(@shared_opts, "Global Options")
end
