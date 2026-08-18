# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Type.Duration do
  @year_month_units [:year, :month]
  @day_time_units [:week, :day, :hour, :minute, :second, :microsecond]
  @duration_units @year_month_units ++ @day_time_units

  @signs [:positive, :negative, :zero]

  @constraints [
    signs: [
      type: {:wrap_list, {:one_of, @signs}},
      doc: """
      The signs the value may have, compared against zero by `Ash.Type.Duration.compare/2`. Any combination is permitted: `:positive` or `[:positive]` requires a positive duration, `[:positive, :zero]` a non-negative one, and `[:positive, :negative]` a non-zero one. Omit the constraint to allow any sign. This is the sign of the duration as a whole, not of each unit — `%Duration{day: 1, hour: -5}` is positive, being nineteen hours. Only where the year/month and week/day sides carry opposite signs does the comparison depend on `compare/2`'s 30-day month.
      """
    ],
    units: [
      type:
        {:or, [{:one_of, [:year_month, :day_time]}, {:wrap_list, {:one_of, @duration_units}}]},
      doc: """
      The units the value may be expressed in. A duration is always re-expressed in the largest of these units that will hold it, on the way in and on the way out, so `[:week, :hour]` turns `1 week 1 day 5 hours` into `1 week 29 hours`. A value that no combination of the permitted units expresses exactly is rejected — including anything that would have to cross the year/month to week/day boundary, which no conversion can. This applies on the way out as well as in: a stored duration the permitted units cannot express is refused rather than quietly rewritten. Either a single unit, an explicit list of them, or a shorthand for one side of that boundary: `:year_month` (`[:year, :month]`) or `:day_time` (`[:week, :day, :hour, :minute, :second, :microsecond]`). Confining an attribute to a single side keeps its values comparable (see `Ash.Type.Duration.compare/2`). With no constraint every unit is permitted, so the same normalization applies and nothing is ever lost.
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
    allowed = permitted_units(constraints[:units])
    normalized = normalize_units(value, allowed)

    case disallowed_units(normalized, allowed) do
      [] ->
        check_sign(normalized, constraints[:signs])

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

  # A magnitude constraint, where `units` is a representation one. Normalizing preserves
  # magnitude, so the two are independent.
  defp check_sign(value, nil), do: {:ok, value}

  defp check_sign(value, permitted) do
    # `wrap_list` normalizes at init, but constraints also arrive here directly.
    permitted = List.wrap(permitted)

    if sign(value) in permitted do
      {:ok, value}
    else
      {:error,
       [
         [
           message: "must be %{signs}",
           signs: Enum.map_join(permitted, " or ", &to_string/1),
           sign: to_string(sign(value))
         ]
       ]}
    end
  end

  defp sign(%Duration{} = value) do
    case compare(value, %Duration{}) do
      :gt -> :positive
      :lt -> :negative
      :eq -> :zero
    end
  end

  # No `units` constraint permits every unit.
  defp permitted_units(nil), do: @duration_units
  defp permitted_units(units), do: expand_units(units)

  defp expand_units(:year_month), do: @year_month_units
  defp expand_units(:day_time), do: @day_time_units
  defp expand_units(unit) when is_atom(unit), do: [unit]
  defp expand_units(units) when is_list(units), do: units

  defp disallowed_units(%Duration{} = value, allowed),
    do: Enum.reject(@duration_units, &(&1 in allowed or unit_zero?(value, &1)))

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

  # A stored value gets the same treatment as one being written: a duration the
  # permitted units cannot express is refused, not quietly rewritten into one they can.
  def cast_stored(value, constraints) when is_binary(value) do
    with {:ok, duration} <- cast_input(value, constraints) do
      apply_constraints(duration, constraints)
    end
  end

  def cast_stored(value, constraints) do
    with {:ok, duration} <- Ecto.Type.load(:duration, value) do
      apply_constraints(duration, constraints)
    end
  end

  @impl true
  def dump_to_embedded(nil, _), do: {:ok, nil}

  def dump_to_embedded(value, _) do
    {:ok, Duration.to_iso8601(value)}
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

  # One canonical form on both sides of storage, whatever units a data layer kept it in.
  defp normalize_units(%Duration{} = duration, allowed) do
    duration
    |> redistribute(@year_month_units, allowed, &total_months/1, &from_months/3)
    |> redistribute(
      @day_time_units,
      allowed,
      &total_microseconds_in_bucket/1,
      &from_microseconds/3
    )
  end

  # Within a bucket only — months and microseconds are not interconvertible. A bucket
  # with no permitted unit has nowhere to put its value, so it is left for
  # `apply_constraints/2` to report, like any other remainder.
  defp redistribute(duration, bucket, allowed, total, build) do
    targets = Enum.filter(bucket, &(&1 in allowed))

    case build.(total.(duration), targets, duration) do
      {:ok, normalized} -> normalized
      :inexact -> duration
    end
  end

  defp total_months(%Duration{year: year, month: month}),
    do: year * @months_per_year + month

  defp total_microseconds_in_bucket(%Duration{
         week: week,
         day: day,
         hour: hour,
         minute: minute,
         second: second,
         microsecond: {microsecond, _precision}
       }) do
    hours = (week * @days_per_week + day) * @hours_per_day + hour
    minutes = hours * @minutes_per_hour + minute
    (minutes * @seconds_per_minute + second) * @usec_per_second + microsecond
  end

  defp from_months(total, targets, duration) do
    with {:ok, assigned} <- assign(total, targets, &months_per_unit/1) do
      {:ok, struct!(duration, Map.merge(%{year: 0, month: 0}, assigned))}
    end
  end

  defp from_microseconds(
         total,
         targets,
         %Duration{microsecond: {_, precision}} = duration
       ) do
    with {:ok, assigned} <- assign(total, targets, &microseconds_per_unit/1) do
      zeroed = %{week: 0, day: 0, hour: 0, minute: 0, second: 0}

      assigned =
        case Map.pop(assigned, :microsecond) do
          {nil, rest} -> Map.put(rest, :microsecond, {0, precision})
          {value, rest} -> Map.put(rest, :microsecond, {value, precision})
        end

      {:ok, struct!(duration, Map.merge(zeroed, assigned))}
    end
  end

  # Largest permitted unit first, so the smallest permitted one carries the rest.
  defp assign(total, targets, size) do
    {assigned, remainder} =
      targets
      |> Enum.sort_by(size, :desc)
      |> Enum.map_reduce(total, fn unit, left ->
        per = size.(unit)
        {{unit, div(left, per)}, rem(left, per)}
      end)

    if remainder == 0 do
      {:ok, Map.new(assigned)}
    else
      :inexact
    end
  end

  defp months_per_unit(:year), do: @months_per_year
  defp months_per_unit(:month), do: 1

  defp microseconds_per_unit(:week), do: @days_per_week * microseconds_per_unit(:day)
  defp microseconds_per_unit(:day), do: @hours_per_day * microseconds_per_unit(:hour)
  defp microseconds_per_unit(:hour), do: @minutes_per_hour * microseconds_per_unit(:minute)
  defp microseconds_per_unit(:minute), do: @seconds_per_minute * @usec_per_second
  defp microseconds_per_unit(:second), do: @usec_per_second
  defp microseconds_per_unit(:microsecond), do: 1

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
