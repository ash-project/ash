defmodule Ash.Flow.Step.Custom do
  @moduledoc "Runs a custom step."
  defstruct [:name, :input, :custom, :async?]
  @shared_opts Ash.Flow.Step.shared_opts()

  def schema do
    [
      input: Ash.Flow.Step.input(),
      custom: [
        type: {:ash_behaviour, Ash.Flow.Step}
      ],
      async?: [
        type: :boolean,
        doc:
          "Whether or not this step can be run outside of the main process. Defaults to false.",
        default: false
      ]
    ]
    |> Ash.OptionsHelpers.merge_schemas(@shared_opts, "Global Options")
  end
end
