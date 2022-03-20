defmodule Ash.Flow.Step.Transaction do
  @moduledoc "Runs a series of steps over a given input."
  use Ash.Flow.Step.BuiltinStep, [:output, steps: []]
  @shared_opts Ash.Flow.Step.shared_opts()

  def schema,
    do:
      [
        output: [
          type: :any,
          doc:
            "Which step or steps to use when constructing the output list. Defaults to the last step."
        ]
      ]
      |> Ash.OptionsHelpers.merge_schemas(@shared_opts, "Global Options")
end
