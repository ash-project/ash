defmodule Ash.Flow.Step.Debug do
  @moduledoc false
  use Ash.Flow.Step.BuiltinStep, [:input]
  @shared_opts Ash.Flow.Step.shared_opts()

  def schema do
    [
      input: Ash.Flow.Step.input()
    ]
    |> Spark.OptionsHelpers.merge_schemas(@shared_opts, "Global Options")
    |> Keyword.delete(:touches_resources)
  end
end
