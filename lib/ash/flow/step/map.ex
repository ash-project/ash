defmodule Ash.Flow.Step.Map do
  @moduledoc "Runs a series of steps over a given input."
  use Ash.Flow.Step.BuiltinStep, [:over, :element, :output, steps: []]
  @shared_opts Ash.Flow.Step.shared_opts()

  def schema,
    do:
      [
        over: [
          type: :any,
          doc:
            "The value to be iterated over. Will be available inside the `map` step as `element(:map_step_name)`"
        ],
        output: [
          type: :atom,
          doc: "Which step to use when constructing the output list. Defaults to the last step."
        ]
      ]
      |> Spark.OptionsHelpers.merge_schemas(@shared_opts, "Global Options")
end
