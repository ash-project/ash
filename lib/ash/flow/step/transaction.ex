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
            "Which step or steps to use when constructing the output list. Defaults to the last step.",
          links: []
        ],
        timeout: [
          type: :timeout,
          doc: "A timeout to apply to the transaction.",
          links: []
        ],
        resource: [
          type: Ash.OptionsHelpers.ash_resource(),
          doc: """
          The Ash resource to use for the transaction.
          """,
          links: []
        ]
      ]
      |> Spark.OptionsHelpers.merge_schemas(@shared_opts, "Global Options")
      |> Keyword.delete(:touches_resources)
end
