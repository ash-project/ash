defmodule Ash.Flow.Step.Map do
  @moduledoc false
  use Ash.Flow.Step.BuiltinStep, [:over, :element, :output, steps: []]
  @shared_opts Ash.Flow.Step.shared_opts()

  def schema,
    do:
      [
        over: [
          type: :any,
          doc:
            "The value to be iterated over. Will be available inside the `map` step as `element(:map_step_name)`",
          links: []
        ],
        output: [
          type: :atom,
          doc: "Which step to use when constructing the output list. Defaults to the last step.",
          links: []
        ]
      ]
      |> Spark.OptionsHelpers.merge_schemas(@shared_opts, "Global Options")
end
