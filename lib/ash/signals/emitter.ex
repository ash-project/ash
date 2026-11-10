# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Signals.Emitter do
  @moduledoc false
  # Options and payload building shared by the change and preparation that `emit` adds.

  @mapping_type {:list, {:or, [:atom, {:tuple, [:atom, :atom]}]}}

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
      doc: """
      When the signal is emitted, which must be the phase the signal declares. Signals are emitted
      `:after_action` by default.
      """
    ],
    fields: [
      type: @mapping_type,
      default: [],
      doc: """
      Signal fields to take from fields of the record (or of a generic action's result), as a
      keyword list of `signal_field: field`. A bare name takes the field with the same name.
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
    ]
  ]

  @doc false
  def schema, do: @schema

  @doc false
  def init(opts) do
    with {:ok, opts} <- Spark.Options.validate(opts, @schema) do
      {:ok, Keyword.update!(opts, :fields, &Ash.Resource.SignalListener.normalize_mapping/1)}
    end
  end

  @doc false
  def signal!(opts) do
    Ash.Signals.Info.signal(opts[:signal_module], opts[:signal]) ||
      raise ArgumentError,
            "#{inspect(opts[:signal_module])} has no signal named `#{inspect(opts[:signal])}`."
  end

  @doc """
  Builds a payload. Unmapped signal fields are read with `get_default`, and `fields` with
  `get_field`, both returning `:error` when there is no such field. `data` is the record before
  the action, for `previous/1` values.
  """
  def payload(opts, get_default, get_field, data) do
    signal = signal!(opts)

    explicit = Keyword.keys(opts[:fields]) ++ Keyword.keys(opts[:values])

    defaults =
      for %{name: name} <- signal.arguments,
          name not in explicit,
          {:ok, value} <- [get_default.(name)],
          into: %{},
          do: {name, value}

    from_fields =
      for {name, field} <- opts[:fields],
          {:ok, value} <- [get_field.(field)],
          into: %{},
          do: {name, value}

    values =
      Map.new(opts[:values], fn
        {name, {:_previous, field}} -> {name, Map.get(data, field)}
        {name, value} -> {name, value}
      end)

    defaults
    |> Map.merge(from_fields)
    |> Map.merge(values)
  end

  @doc "Returns the fields that `previous/1` values refer to."
  def previous_fields(opts) do
    for {_name, {:_previous, field}} <- opts[:values] || [], do: field
  end
end
