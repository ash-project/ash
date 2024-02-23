defmodule Ash.Flow.Step.Custom do
  @moduledoc "Represents a custom step in an Ash.Flow"
  use Ash.Flow.Step.BuiltinStep, [:input, :custom, :tenant, :async?]
  @shared_opts Ash.Flow.Step.shared_opts()

  def schema do
    [
      input: Ash.Flow.Step.input(),
      custom: [
        type: {:spark_function_behaviour, Ash.Flow.Step, {Ash.Flow.Step.CustomFunction, 2}},
        doc:
          "The module that implements the step behaviour. Also accepts a 2 argument function that takes the input and the context."
      ],
      async?: [
        type: :boolean,
        doc: """
        Whether or not this step can be run outside of the current process.
        """,
        default: false
      ]
    ]
    |> Spark.Options.merge(@shared_opts, "Global Options")
  end
end
