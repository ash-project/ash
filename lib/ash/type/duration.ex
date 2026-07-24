# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Type.Duration do
  @year_month_units [:year, :month]
  @day_time_units [:week, :day, :hour, :minute, :second, :microsecond]
  @duration_units @year_month_units ++ @day_time_units

  @constraints [
    units: [
      type: {:or, [{:in, [:year_month, :day_time]}, {:list, {:in, @duration_units}}]},
      doc: """
      The units permitted to be non-zero; any unit outside the set must be zero, otherwise casting fails. Either an explicit list of units, or a shorthand for one side of the comparability boundary: `:year_month` (`[:year, :month]`) or `:day_time` (`[:week, :day, :hour, :minute, :second, :microsecond]`). Confining an attribute to a single side keeps its values comparable (see `Ash.Type.Duration.compare/2`).
      """
    ]
  ]

  @moduledoc """
  Represents a Duration

  A builtin type that can be referenced via `:duration`

  ### Constraints

  #{Spark.Options.docs(@constraints)}
  """
  use Ash.Type

  @impl true
  def constraints, do: @constraints

  @impl true
  def storage_type(_), do: :duration

  @impl true
  def generator(_constraints) do
    # Waiting on blessed date/datetime generators in stream data
    # https://github.com/whatyouhide/stream_data/pull/161/files
    StreamData.constant(Duration.new!(minute: 30))
  end

  @impl true
  def cast_input(nil, _), do: {:ok, nil}

  def cast_input(value, _) do
    case Ecto.Type.cast(:duration, value) do
      :error ->
        if is_binary(value) do
          case Duration.from_iso8601(value) do
            {:ok, duration} -> {:ok, duration}
            {:error, error} -> {:error, error}
          end
        else
          :error
        end

      {:error, error} ->
        if is_binary(value) do
          case Duration.from_iso8601(value) do
            {:ok, duration} -> {:ok, duration}
            {:error, _} -> {:error, error}
          end
        else
          {:error, error}
        end

      {:ok, duration} ->
        {:ok, duration}
    end
  end

  @impl true
  def apply_constraints(nil, _), do: {:ok, nil}

  def apply_constraints(%Duration{} = value, constraints) do
    case constraints[:units] do
      nil ->
        {:ok, value}

      units ->
        allowed = expand_units(units)

        case Enum.reject(@duration_units, &(&1 in allowed or unit_zero?(value, &1))) do
          [] ->
            {:ok, value}

          disallowed ->
            {:error,
             [
               [
                 message: "must only use the units %{units}",
                 units: Enum.map_join(allowed, ", ", &to_string/1),
                 disallowed: Enum.map_join(disallowed, ", ", &to_string/1)
               ]
             ]}
        end
    end
  end

  defp expand_units(:year_month), do: @year_month_units
  defp expand_units(:day_time), do: @day_time_units
  defp expand_units(units) when is_list(units), do: units

  defp unit_zero?(%Duration{microsecond: {value, _precision}}, :microsecond), do: value == 0
  defp unit_zero?(%Duration{} = duration, unit), do: Map.fetch!(duration, unit) == 0

  @impl true
  def matches_type?(%{__struct__: Duration}, _), do: true
  def matches_type?(_, _), do: false

  @impl true
  def cast_atomic(new_value, _constraints) do
    {:atomic, new_value}
  end

  # A `units` whitelist is a property of the decoded `Duration` struct's fields,
  # which cannot be checked within an atomic expression. Fall back to the
  # non-atomic path so `apply_constraints/2` enforces it.
  @impl true
  def may_support_atomic_update?(constraints), do: is_nil(constraints[:units])

  @impl true
  def cast_stored(nil, _), do: {:ok, nil}

  def cast_stored(value, constraints) when is_binary(value) do
    cast_input(value, constraints)
  end

  def cast_stored(value, _) do
    Ecto.Type.load(:duration, value)
  end

  @impl true
  def dump_to_embedded(nil, _), do: {:ok, nil}

  def dump_to_embedded(value, _) do
    Duration.to_iso8601(value)
  end

  @impl true
  def dump_to_native(nil, _), do: {:ok, nil}

  def dump_to_native(value, _) do
    Ecto.Type.dump(:duration, value)
  end
end
