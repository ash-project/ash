# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Query.Function.StartOfDay do
  @moduledoc """
  Converts a date or datetime into the start of day

  Accepts an optional time zone, in the same format that can be passed to
  `DateTime.new/3`.

  For example:
     start_of_day(now()) < a_datetime()
     start_of_day(now(), "Europe/Copenhagen") < a_datetime()
  """

  use Ash.Query.Function, name: :start_of_day

  def args, do: [[:datetime], [:datetime, :string], [:date], [:date, :string]]

  def returns, do: [:utc_datetime, :utc_datetime, :utc_datetime, :utc_datetime]

  def evaluate(%{arguments: [%DateTime{} = date]}) do
    with {:ok, term} <- DateTime.new(DateTime.to_date(date), Time.new!(0, 0, 0)) do
      {:known, term}
    end
  end

  def evaluate(%{arguments: [%DateTime{} = date, timezone]}) do
    with {:ok, term} <-
           DateTime.new(
             DateTime.to_date(DateTime.shift_zone!(date, timezone)),
             Time.new!(0, 0, 0),
             timezone
           ) do
      {:known, DateTime.shift_zone!(term, "Etc/UTC")}
    end
  end

  def evaluate(%{arguments: [date]}) do
    with {:ok, term} <- DateTime.new(date, Time.new!(0, 0, 0)) do
      {:known, term}
    end
  end

  def evaluate(%{arguments: [date, timezone]}) do
    with {:ok, term} <- DateTime.new(date, Time.new!(0, 0, 0), timezone) do
      {:known, DateTime.shift_zone!(term, "Etc/UTC")}
    end
  end

  def can_return_nil?(_), do: false
end
