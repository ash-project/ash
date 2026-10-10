# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Signals.Signal do
  @moduledoc "Represents a `signal` declared in an `Ash.Signals` module."
  defstruct [
    :name,
    :phase,
    :description,
    :struct,
    arguments: [],
    __spark_metadata__: nil
  ]

  @type phase :: :before_action | :after_action | :after_transaction

  @type t :: %__MODULE__{
          name: atom(),
          phase: phase(),
          description: String.t() | nil,
          struct: module(),
          arguments: [Ash.Signals.Field.t()],
          __spark_metadata__: Spark.Dsl.Entity.spark_meta()
        }

  @schema [
    name: [
      type: :atom,
      required: true,
      doc: "The name of the signal."
    ],
    phase: [
      type: {:one_of, [:before_action, :after_action, :after_transaction]},
      required: true,
      doc: "The phase of the emitting action during which the signal may be emitted."
    ],
    description: [
      type: :string,
      doc: "A description of the signal."
    ]
  ]

  @doc false
  def schema, do: @schema
end
