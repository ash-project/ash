# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Query.Function.DateTimeAdd do
  @moduledoc """
  Adds the given interval or Duration to the current time in UTC

  For example:
     activates_at < datetime_add(now(), 7, :day)
     activates_at < datetime_add(now(), Duration.new!(day:7))

  Documentation + available intervals inspired by the corresponding ecto interval implementation
  """

  use Ash.Query.Function, name: :datetime_add, eager_evaluate?: false

  def args, do: [[:utc_datetime, :integer, :duration_name], [:utc_datetime, :duration]]

  def returns, do: [:utc_datetime]

  def evaluate(%{arguments: [datetime, factor, interval]}) do
    shifted = Ash.Query.Function.Ago.datetime_add(datetime, factor, interval)
    {:known, shifted}
  end

  def evaluate(%{arguments: [datetime, duration]}) when is_struct(duration, Duration) do
    shifted = DateTime.shift(datetime, duration)
    {:known, shifted}
  end

  def can_return_nil?(%{arguments: [datetime | _]}) do
    Ash.Expr.can_return_nil?(datetime)
  end
end
