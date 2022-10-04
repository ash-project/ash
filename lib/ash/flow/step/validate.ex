defmodule Ash.Flow.Step.Validate do
  @moduledoc false
  use Ash.Flow.Step.BuiltinStep, [:resource, :record, :input, :action, :api, :tenant, :only_keys]
  @shared_opts Ash.Flow.Step.shared_opts()
  @shared_action_opts Ash.Flow.Step.shared_action_opts()

  def schema,
    do:
      [
        record: [
          type: :any,
          doc: """
          The record to be created/updated/destroyed, if relevant. can use template helpers, e.g `result(:step_name)`.

          If the value is `nil` and would be required by the action type, the step is skipped and `nil` is the result of the step.
          Any other value is used as an input record.
          """,
          links: []
        ],
        only_keys: [
          type: {:list_of, {:or, [:atom, {:list_of, :atom}]}},
          doc: """
          If the keys are set, the step will succeed as long as there are no errors for those specific fields.
          Additionally, only errors for those keys will be returned.
          Use a list for the key if you want to check for an error at a path, and use `:_` to allow anything at that path
          """,
          links: []
        ]
      ]
      |> Spark.OptionsHelpers.merge_schemas(@shared_opts, "Global Options")
      |> Spark.OptionsHelpers.merge_schemas(@shared_action_opts, "Action Step Opts")
end
