defmodule Ash.Flow.Step.Transaction do
  @moduledoc "Represents steps grouped into a transaction in an Ash.Flow"
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
          type: {:wrap_list, Ash.OptionsHelpers.ash_resource()},
          doc: """
          The Ash resource to use for the transaction.
          """
        ]
      ]
      |> Spark.Options.merge(@shared_opts, "Global Options")
end
