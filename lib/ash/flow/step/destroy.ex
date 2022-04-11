defmodule Ash.Flow.Step.Destroy do
  @moduledoc "A flow step to run a destroy action."
  use Ash.Flow.Step.BuiltinStep, [:resource, :action, :api, :input, :tenant, :record]
  @shared_opts Ash.Flow.Step.shared_opts()
  @shared_action_opts Ash.Flow.Step.shared_action_opts()

  def schema,
    do:
      [
        record: [
          type: :any,
          required: true,
          doc: """
          The record to be updated, can use template helpers, e.g `result(:step_name)`.

          If the value is `nil`, the step is skipped and `nil` is the result of the step.
          Any other value is used as an input record.
          """
        ]
      ]
      |> Ash.OptionsHelpers.merge_schemas(@shared_opts, "Global Options")
      |> Ash.OptionsHelpers.merge_schemas(@shared_action_opts, "Action Step Opts")
end
