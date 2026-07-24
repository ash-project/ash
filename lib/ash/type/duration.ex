# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Type.Duration do
  @moduledoc """
  Represents a Duration

  A builtin type that can be referenced via `:duration`
  """
  use Ash.Type

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
  def matches_type?(%{__struct__: Duration}, _), do: true
  def matches_type?(_, _), do: false

  @impl true
  def cast_atomic(new_value, _constraints) do
    {:atomic, new_value}
  end

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

  @microseconds_per %{
    second: 1_000_000,
    minute: 60 * 1_000_000,
    hour: 60 * 60 * 1_000_000,
    day: 24 * 60 * 60 * 1_000_000,
    week: 7 * 24 * 60 * 60 * 1_000_000
  }

  @months_per_year 12

  @doc """
  Compares two durations within a single ordered regime.

  `Duration` as a whole is only *partially* ordered — a month is not a fixed
  number of days, so `P1M` versus `P30D` has no context-free answer, which is why
  Elixir ships `Duration` without a `compare/2`. But it splits into two regimes
  that each *are* totally ordered, mirroring `xs:yearMonthDuration` and
  `xs:dayTimeDuration` in XML Schema / XPath:

    * `:day_time` — `week`, `day`, `hour`, `minute`, `second`, `microsecond`,
      normalised to microseconds (`week` = 7 days, `day` = 24 hours) at full
      precision, so `PT1.000000S` and `PT1.000500S` stay distinct (unlike
      `to_timeout/1`, which truncates to milliseconds).
    * `:year_month` — `year`, `month`, normalised to months (`year` = 12 months).

  Two durations are comparable only when they share a regime; a wholly-zero
  duration belongs to either. Comparing across the regimes, or comparing a
  duration that mixes units from both, raises `Ash.Error.Query.InvalidExpression`
  because no context-free order exists.

  This function is the single place the `Duration` boundary lives; if Elixir core
  later gains a `Duration.compare/2` with a matching boundary, it can delegate here.
  """
  @spec compare(Duration.t(), Duration.t()) :: :lt | :eq | :gt
  def compare(%Duration{} = left, %Duration{} = right) do
    case {regime(left), regime(right)} do
      {:mixed, _} -> raise_incomparable(left, right)
      {_, :mixed} -> raise_incomparable(left, right)
      {:year_month, :day_time} -> raise_incomparable(left, right)
      {:day_time, :year_month} -> raise_incomparable(left, right)
      {:zero, :zero} -> :eq
      {left_regime, right_regime} -> compare_within(left_regime, right_regime, left, right)
    end
  end

  # After the mixed/cross-regime/zero-zero cases above, any remaining pair shares
  # a regime (a `:zero` operand takes the other's), so pick the normalisation from
  # whichever operand is not `:zero`.
  defp compare_within(left_regime, right_regime, left, right) do
    if :year_month in [left_regime, right_regime] do
      compare_ints(total_months(left), total_months(right))
    else
      compare_ints(total_microseconds(left), total_microseconds(right))
    end
  end

  defp regime(%Duration{
         year: year,
         month: month,
         week: week,
         day: day,
         hour: hour,
         minute: minute,
         second: second,
         microsecond: {microsecond, _precision}
       }) do
    year_month? = year != 0 or month != 0

    day_time? =
      week != 0 or day != 0 or hour != 0 or minute != 0 or second != 0 or microsecond != 0

    cond do
      year_month? and day_time? -> :mixed
      year_month? -> :year_month
      day_time? -> :day_time
      true -> :zero
    end
  end

  defp total_months(%Duration{year: year, month: month}), do: year * @months_per_year + month

  defp total_microseconds(%Duration{
         week: week,
         day: day,
         hour: hour,
         minute: minute,
         second: second,
         microsecond: {microsecond, _precision}
       }) do
    week * @microseconds_per.week +
      day * @microseconds_per.day +
      hour * @microseconds_per.hour +
      minute * @microseconds_per.minute +
      second * @microseconds_per.second +
      microsecond
  end

  defp compare_ints(left, right) do
    cond do
      left > right -> :gt
      left < right -> :lt
      true -> :eq
    end
  end

  defp raise_incomparable(left, right) do
    raise Ash.Error.Query.InvalidExpression.exception(
            expression: {left, right},
            message:
              "durations are only partially ordered: #{inspect(left)} and #{inspect(right)} do " <>
                "not share an ordered regime (year/month versus day/time, or a duration mixing " <>
                "both), so no context-free comparison exists — mirroring xs:yearMonthDuration " <>
                "versus xs:dayTimeDuration"
          )
  end
end

import Ash.Type.Comparable

defcomparable left :: Duration, right :: Duration do
  Ash.Type.Duration.compare(left, right)
end
