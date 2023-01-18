defmodule Ash.Flow.Step.Transaction do
  @moduledoc false
  use Ash.Flow.Step.BuiltinStep, [:output, :resource, :timeout, steps: []]
  @shared_opts Ash.Flow.Step.shared_opts()

  def schema,
    do:
      [
        output: [
          type: :any,
          doc:
            "Which step or steps to use when constructing the output. Defaults to the last step."
        ],
        timeout: [
          type: :timeout,
          doc: "A timeout to apply to the transaction."
        ],
        resource: [
          type:
            {:or, [Ash.OptionsHelpers.ash_resource(), {:list, Ash.OptionsHelpers.ash_resource()}]},
          doc: """
          The Ash resource to use for the transaction.
          """
        ]
      ]
      |> Spark.OptionsHelpers.merge_schemas(@shared_opts, "Global Options")
end
