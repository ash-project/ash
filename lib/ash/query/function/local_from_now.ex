# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Query.Function.LocalFromNow do
  @moduledoc """
  Adds an interval or duration to the current UTC time, then converts the result to a
  `NaiveDateTime` in the given time zone.

  For example, `local_from_now(7, :day, "Asia/Hong_Kong")` or
  `local_from_now(Duration.new!(day: 7), "Asia/Hong_Kong")`. Temporal queries use
  their `as_of` instant.
  """
  use Ash.Query.Function, name: :local_from_now, eager_evaluate?: false

  alias Ash.Query.Function.{Ago, LocalNow}

  def args, do: [[:integer, :duration_name, :string], [:duration, :string]]

  def returns, do: [:naive_datetime, :naive_datetime]

  def evaluate(%{arguments: [factor, interval, time_zone]} = function) do
    function
    |> LocalNow.utc_now()
    |> Ago.datetime_add(factor, interval)
    |> LocalNow.to_local(time_zone)
  end

  def evaluate(%{arguments: [duration, time_zone]} = function) do
    function
    |> LocalNow.utc_now()
    |> Ago.datetime_add(duration)
    |> LocalNow.to_local(time_zone)
  end
end
