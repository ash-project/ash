defmodule Ash.Flow.Step.Custom do
  @moduledoc "Runs a custom step."
  use Ash.Flow.Step.BuiltinStep, [:input, :custom, :async?]
  @shared_opts Ash.Flow.Step.shared_opts()

  def schema do
    [
      input: Ash.Flow.Step.input(),
      custom: [
        type: {:ash_behaviour, Ash.Flow.Step}
      ],
      async?: [
        type: :boolean,
        doc: """
        Whether or not this step can be run outside of the current process. Defaults to true.

        Generally speaking, you should also set the `touches_resources` if you set `async?` to true.
        This ensures that the custom step will be run synchronously if any of those resource's data
        layers is in a corresponding transaction. You don't necessarily need to set *all* of the
        resources that will be touched. For example, all AshPostgres resources that share the same
        repo share the same transaction state.
        """,
        default: false
      ]
    ]
    |> Ash.OptionsHelpers.merge_schemas(@shared_opts, "Global Options")
  end
end
