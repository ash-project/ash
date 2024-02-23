defmodule Ash.Flow.Step.Debug do
  @moduledoc "Represents a debug step in an Ash.Flow"
  use Ash.Flow.Step.BuiltinStep, [:input]
  @shared_opts Ash.Flow.Step.shared_opts()

  def schema do
    [
      input: Ash.Flow.Step.input()
    ]
    |> Spark.Options.merge(@shared_opts, "Global Options")
    |> Keyword.delete(:touches_resources)
  end
end
