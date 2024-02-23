defmodule Ash.Flow.Step.Map do
  @moduledoc "Represents a map grouping of steps in an Ash.Flow"
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
      |> Spark.Options.merge(@shared_opts, "Global Options")
end
