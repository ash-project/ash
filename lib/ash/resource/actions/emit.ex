# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Resource.Actions.Emit do
  @moduledoc "Represents an `emit` declaration on an action."
  defstruct [
    :signal_module,
    :signal,
    :description,
    phase: :after_action,
    fields: [],
    values: [],
    where: [],
    dynamic?: false,
    __spark_metadata__: nil
  ]

  @type t :: %__MODULE__{
          signal_module: Ash.Signals.t(),
          signal: atom(),
          description: String.t() | nil,
          phase: Ash.Signals.Signal.phase(),
          fields: [{atom(), atom()}],
          values: Keyword.t(),
          where: [Ash.Resource.Validation.ref()],
          dynamic?: boolean(),
          __spark_metadata__: Spark.Dsl.Entity.spark_meta()
        }

  @schema [
    signal_module: [
      type: :atom,
      required: true,
      doc: "The `Ash.Signals` module that declares the signal."
    ],
    signal: [
      type: :atom,
      required: true,
      doc: "The name of the signal."
    ],
    phase: [
      type: {:one_of, [:before_action, :after_action, :after_transaction]},
      default: :after_action,
      doc: "When the signal is emitted, which must be the phase the signal declares."
    ],
    fields: [
      type: {:list, {:or, [:atom, {:tuple, [:atom, :atom]}]}},
      default: [],
      doc: """
      Signal fields to take from attributes of the record (or, in a generic action, from fields of
      its result), as a keyword list of `signal_field: field`. A bare name takes the field with the
      same name.
      """
    ],
    values: [
      type: :keyword_list,
      default: [],
      doc: """
      Signal fields to set to the given values, which may be templates like `arg(:reason)` or
      `actor(:id)`, or for update and destroy actions `previous(:field)`, the value of a field
      before the action.
      """
    ],
    where: [
      type:
        {:wrap_list,
         {:spark_function_behaviour, Ash.Resource.Validation, Ash.Resource.Validation.Builtins,
          {Ash.Resource.Validation.Function, 2}}},
      default: [],
      doc: "Validations that must pass for the signal to be emitted."
    ],
    dynamic?: [
      type: :boolean,
      default: false,
      doc: """
      Declares that the action emits the signal from your own code, with `Ash.Signals.emit/5` or
      `Ash.Signals.emit_many/5`, instead of emitting it. Use it when building the signal needs
      more than `fields` and `values` can express.
      """
    ],
    description: [
      type: :string,
      doc: """
      A description of when and why the action emits the signal. Especially useful with
      `dynamic?: true`, where the code that emits the signal decides when it is emitted.
      """
    ]
  ]

  @doc false
  def schema, do: @schema

  @doc false
  def transform(emit) do
    {:ok, %{emit | fields: Ash.Resource.SignalListener.normalize_mapping(emit.fields)}}
  end

  @doc false
  # The options of the `Ash.Resource.Change.EmitSignal` change, or the
  # `Ash.Resource.Preparation.EmitSignal` preparation, that emit the signal.
  def emitter_opts(emit) do
    [
      signal_module: emit.signal_module,
      signal: emit.signal,
      phase: emit.phase,
      fields: emit.fields,
      values: emit.values
    ]
  end
end
