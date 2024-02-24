defmodule Ash.Flow.Step.Destroy do
  @moduledoc "Represents a destroy action step in an Ash.Flow"
  use Ash.Flow.Step.BuiltinStep, [:resource, :action, :domain, :input, :tenant, :record]
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
      |> Spark.Options.merge(@shared_opts, "Global Options")
      |> Spark.Options.merge(@shared_action_opts, "Action Step Opts")
end
