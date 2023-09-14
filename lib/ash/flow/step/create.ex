defmodule Ash.Flow.Step.Create do
  @moduledoc "Represents a create action step in an Ash.Flow"
  use Ash.Flow.Step.BuiltinStep, [
    :resource,
    :action,
    :api,
    :tenant,
    :input,
    :upsert?,
    :upsert_identity
  ]

  @shared_opts Ash.Flow.Step.shared_opts()
  @shared_action_opts Ash.Flow.Step.shared_action_opts()

  def schema,
    do:
      [
        upsert?: [
          type: :boolean,
          doc: "Wether or not this action is always an upsert.",
          default: false
        ],
        upsert_identity: [
          type: :atom,
          doc: "The identity to use for the upsert."
        ]
      ]
      |> Spark.OptionsHelpers.merge_schemas(@shared_opts, "Global Options")
      |> Spark.OptionsHelpers.merge_schemas(@shared_action_opts, "Action Step Opts")
end
