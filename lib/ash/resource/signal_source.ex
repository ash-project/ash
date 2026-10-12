# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Resource.SignalSource do
  @moduledoc "Represents a `from` declaration in the `signals_in` section of a resource."
  defstruct [:signal_module, listeners: [], __spark_metadata__: nil]

  @type t :: %__MODULE__{
          signal_module: Ash.Signals.t(),
          listeners: [Ash.Resource.SignalListener.t()],
          __spark_metadata__: Spark.Dsl.Entity.spark_meta()
        }

  @schema [
    signal_module: [
      type: :atom,
      required: true,
      doc: "The `Ash.Signals` module that declares the signals."
    ]
  ]

  @doc false
  def schema, do: @schema
end
