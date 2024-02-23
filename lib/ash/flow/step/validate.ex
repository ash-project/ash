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
          The record to be created/updated/destroyed. If the value is `nil` and would be required by the action type, the step is skipped and `nil` is the result of the step.
          """
        ],
        only_keys: [
          type: {:list, {:wrap_list, :atom}},
          doc: """
          A list of keys or paths to keys that should be validated. Others will be ignored, and errors generated for other fields will be ignored.
          """
        ]
      ]
      |> Spark.Options.merge(@shared_opts, "Global Options")
      |> Spark.Options.merge(@shared_action_opts, "Action Step Opts")
end
