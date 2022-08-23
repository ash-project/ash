defmodule Ash.Flow.Step.Create do
  @moduledoc false
  use Ash.Flow.Step.BuiltinStep, [:resource, :action, :api, :tenant, :input]
  @shared_opts Ash.Flow.Step.shared_opts()
  @shared_action_opts Ash.Flow.Step.shared_action_opts()

  def schema,
    do:
      []
      |> Spark.OptionsHelpers.merge_schemas(@shared_opts, "Global Options")
      |> Spark.OptionsHelpers.merge_schemas(@shared_action_opts, "Action Step Opts")
end
