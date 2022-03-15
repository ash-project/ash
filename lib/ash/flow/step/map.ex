defmodule Ash.Flow.Step.Map do
  @moduledoc "Runs a series of steps over a given input."
  defstruct [:name, :over, :element, :output, steps: []]
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
          type: :any,
          doc:
            "Which step or steps to use when constructing the output list. Defaults to the last step."
        ]
      ]
      |> Ash.OptionsHelpers.merge_schemas(@shared_opts, "Global Options")
end
