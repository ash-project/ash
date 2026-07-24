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

  @usec_per_second 1_000_000
  @seconds_per_minute 60
  @minutes_per_hour 60
  @hours_per_day 24
  @days_per_week 7
  @days_per_month 30
  @months_per_year 12

  @doc """
  Compares two durations as a total order, matching how the AshPostgres data
  layer (PostgreSQL `interval`) compares them: a fixed conversion of `month` → 30
  days and `day` → 24 hours (so `year` → 360 days, `week` → 7 days), down to
  microseconds.

  `Duration` is only *partially* ordered in general — a month is not a fixed
  number of days — which is why Elixir ships `Duration` without a `compare/2`, and
  why data layers disagree on cross-unit comparison: PostgreSQL uses 30-day
  months, Neo4j ~30.44-day months, and Elixir's `to_timeout/1` refuses `month`/
  `year` outright. This adopts PostgreSQL's convention so in-memory comparison
  stays aligned with the dominant data layer rather than raising or drifting.
  Within the day/time units, or within the year/month units, the result is exact
  and portable across those backends; only comparison *across* that boundary
  depends on the 30-day convention.

  Computed from the integer fields directly, so microsecond precision is kept
  (unlike `to_timeout/1`, which truncates to milliseconds).

  This function is the single place the convention lives; if Elixir core later
  gains a `Duration.compare/2`, it can delegate here.
  """
  @spec compare(Duration.t(), Duration.t()) :: :lt | :eq | :gt
  def compare(%Duration{} = left, %Duration{} = right) do
    compare_ints(total_microseconds(left), total_microseconds(right))
  end

  defp total_microseconds(%Duration{
         year: year,
         month: month,
         week: week,
         day: day,
         hour: hour,
         minute: minute,
         second: second,
         microsecond: {microsecond, _precision}
       }) do
    days = (year * @months_per_year + month) * @days_per_month + week * @days_per_week + day
    hours = days * @hours_per_day + hour
    minutes = hours * @minutes_per_hour + minute
    seconds = minutes * @seconds_per_minute + second
    seconds * @usec_per_second + microsecond
  end

  defp compare_ints(left, right) do
    cond do
      left > right -> :gt
      left < right -> :lt
      true -> :eq
    end
  end
end

import Ash.Type.Comparable

defcomparable left :: Duration, right :: Duration do
  Ash.Type.Duration.compare(left, right)
end
