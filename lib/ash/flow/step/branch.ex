defmodule Ash.Flow.Step.Branch do
  @moduledoc false
  use Ash.Flow.Step.BuiltinStep, [:output, :condition, steps: []]
  @shared_opts Ash.Flow.Step.shared_opts()

  def schema,
    do:
      [
        condition: [
          type: :any,
          doc: "A template that must evaluate to `true` for the branch to be executed.",
          links: []
        ],
        output: [
          type: :atom,
          doc: "Which step to use as the output. Defaults to the last step.",
          links: []
        ]
      ]
      |> Spark.OptionsHelpers.merge_schemas(@shared_opts, "Global Options")
end
