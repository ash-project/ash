defmodule Ash.Flow.Step.Debug do
  @moduledoc "IO.puts() the input provided, and any other debug information."
  use Ash.Flow.Step.BuiltinStep, [:input]
  @shared_opts Ash.Flow.Step.shared_opts()

  def schema do
    [
      input: Ash.Flow.Step.input()
    ]
    |> Ash.OptionsHelpers.merge_schemas(@shared_opts, "Global Options")
    |> Keyword.delete(:touches_resources)
  end
end
