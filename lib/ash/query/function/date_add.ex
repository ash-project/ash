# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Query.Function.DateAdd do
  @moduledoc """
  Adds the given interval or Duration to a date

  For example:
     activates_at < date_add(today(), 7, :day)
     activates_at < date_add(today(), Duration.new!(day: 7))

  When every argument is known (no references, no `today()`), the result is computed
  before the expression reaches the data layer, so it is sent as a plain value.

  Documentation + available intervals inspired by the corresponding ecto interval implementation
  """

  use Ash.Query.Function, name: :date_add

  @beginning_of_day Time.new!(0, 0, 0)

  def args, do: [[:date, :integer, :duration_name], [:date, :duration]]

  def returns, do: [:date]

  def evaluate(%{arguments: [date, factor, interval]}) do
    with {:ok, datetime} <- DateTime.new(date, @beginning_of_day),
         shifted <- Ash.Query.Function.Ago.datetime_add(datetime, factor, interval),
         truncated <- DateTime.to_date(shifted) do
      {:known, truncated}
    end
  end

  def evaluate(%{arguments: [date, duration]}) when is_struct(duration, Duration) do
    shifted = Date.shift(date, duration)
    {:known, shifted}
  end

  def can_return_nil?(%{arguments: [date | _]}) do
    Ash.Expr.can_return_nil?(date)
  end
end
