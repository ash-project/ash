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
          required: true,
          links: []
        ],
        input: Ash.Flow.Step.input()
      ]
      |> Spark.OptionsHelpers.merge_schemas(@shared_opts, "Global Options")
end
