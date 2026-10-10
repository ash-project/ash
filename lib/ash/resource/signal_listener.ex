# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Resource.SignalListener do
  @moduledoc "Represents an `on` declaration in the `signals_in` section of a resource."
  defstruct [
    :signal_module,
    :signal,
    :action,
    :action_type,
    :resource,
    :description,
    :argument,
    :inputs,
    :read_action,
    args: [],
    get_by: [],
    batch?: false,
    __spark_metadata__: nil
  ]

  @type mapping :: [{atom(), atom()}]

  @type t :: %__MODULE__{
          signal_module: Ash.Signals.t(),
          signal: atom(),
          action: atom(),
          action_type: Ash.Resource.Actions.action_type() | nil,
          resource: Ash.Resource.t(),
          argument: atom() | nil,
          inputs: mapping() | nil,
          read_action: atom() | nil,
          args: mapping(),
          get_by: mapping(),
          batch?: boolean(),
          description: String.t() | nil,
          __spark_metadata__: Spark.Dsl.Entity.spark_meta()
        }

  @mapping_doc """
  A keyword list mapping each name to the signal field it takes its value from. A bare name takes
  its value from the signal field with the same name, e.g. `[:customer_id, order: :order_id]`.
  """

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
    action: [
      type: :atom,
      required: true,
      doc: """
      The action to run when the signal is emitted. A generic action receives the signal struct as
      an argument. A create action creates a record from each signal. An update or destroy action
      runs on the records that `read_action`, `args` and `get_by` select for each signal.
      """
    ],
    batch?: [
      type: :boolean,
      default: false,
      doc: """
      For a generic action, run it once with every signal emitted together (by
      `Ash.Signals.emit_many/5`), as a list, instead of once per signal. Create, update and destroy
      actions always handle signals in bulk.
      """
    ],
    argument: [
      type: :atom,
      doc: """
      For a generic action, the argument that receives the signal struct. Defaults to `:signal`,
      or to `:signals` with `batch?: true`, in which case it receives a list of signal structs.
      """
    ],
    inputs: [
      type: {:list, {:or, [:atom, {:tuple, [:atom, :atom]}]}},
      doc: """
      For a create, update or destroy action, the inputs of the action to set from the signal.
      Defaults to every signal field with the same name as an input of the action.
      #{@mapping_doc}
      """
    ],
    read_action: [
      type: :atom,
      doc: """
      For an update or destroy action, the read action that selects the records to change.
      Defaults to the primary read action. Signals for which it returns no records are handled.
      """
    ],
    args: [
      type: {:list, {:or, [:atom, {:tuple, [:atom, :atom]}]}},
      default: [],
      doc: """
      For an update or destroy action, the arguments of the read action to set from the signal.
      #{@mapping_doc}
      """
    ],
    get_by: [
      type: {:list, {:or, [:atom, {:tuple, [:atom, :atom]}]}},
      default: [],
      doc: """
      For an update or destroy action, fields of the resource that must equal the given signal
      fields. #{@mapping_doc}
      """
    ],
    description: [
      type: :string,
      doc: "A description of the listener."
    ]
  ]

  @doc false
  def schema, do: @schema

  @doc false
  # Normalizes a mapping option to a keyword list of `name => signal_field`.
  def normalize_mapping(nil), do: nil

  def normalize_mapping(mapping) do
    Enum.map(mapping, fn
      {name, field} -> {name, field}
      name -> {name, name}
    end)
  end
end
