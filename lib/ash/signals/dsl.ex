# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Signals.Dsl do
  @moduledoc false

  @argument %Spark.Dsl.Entity{
    name: :argument,
    describe: "An argument of the signal, provided by the emitter.",
    examples: ["argument :post_id, :uuid, allow_nil?: false"],
    target: Ash.Signals.Field,
    schema: Ash.Signals.Field.schema(),
    transform: {Ash.Signals.Field, :transform, []},
    args: [:name, :type]
  }

  @signal %Spark.Dsl.Entity{
    name: :signal,
    describe: """
    Declares a signal, and defines a struct for its arguments.

    For a signal named `:post_created` in `MyApp.Signals`, this is `MyApp.Signals.PostCreated`.
    """,
    examples: [
      """
      signal :post_created do
        phase :after_action
        argument :post_id, :uuid, allow_nil?: false
      end
      """
    ],
    target: Ash.Signals.Signal,
    schema: Ash.Signals.Signal.schema(),
    entities: [
      arguments: [@argument]
    ],
    args: [:name]
  }

  @signals %Spark.Dsl.Section{
    name: :signals,
    describe: "Declare signals that resources can emit and listen to.",
    examples: [
      """
      signals do
        signal :post_created do
          phase :after_action
          argument :post_id, :uuid, allow_nil?: false
        end
      end
      """
    ],
    entities: [@signal]
  }

  use Spark.Dsl.Extension,
    sections: [@signals],
    transformers: [Ash.Signals.Transformers.DefineStructs]
end
