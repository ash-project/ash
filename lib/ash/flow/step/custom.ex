defmodule Ash.Flow.Step.Custom do
  @moduledoc false
  use Ash.Flow.Step.BuiltinStep, [:input, :custom, :async?]
  @shared_opts Ash.Flow.Step.shared_opts()

  def schema do
    [
      input: Ash.Flow.Step.input(),
      custom: [
        type: {:spark_behaviour, Ash.Flow.Step},
        doc: "The module that implements the step behaviour",
        links: []
      ],
      async?: [
        type: :boolean,
        doc: """
        Whether or not this step can be run outside of the current process. Defaults to true.
        """,
        links: [],
        default: false
      ]
    ]
    |> Spark.OptionsHelpers.merge_schemas(@shared_opts, "Global Options")
  end
end
