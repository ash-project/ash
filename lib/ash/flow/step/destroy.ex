defmodule Ash.Flow.Step.Destroy do
  @moduledoc "Represents a destroy action step in an Ash.Flow"
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
          """
        ]
      ]
      |> Spark.OptionsHelpers.merge_schemas(@shared_opts, "Global Options")
      |> Spark.OptionsHelpers.merge_schemas(@shared_action_opts, "Action Step Opts")
end
