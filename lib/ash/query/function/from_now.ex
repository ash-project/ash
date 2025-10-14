# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Query.Function.FromNow do
  @moduledoc """
  Adds the given interval or Duration from the current time in UTC.

  For example:
     expires_at < from_now(7, :day)
     expires_at < from_now(Duration.new!(day: 7))

  Documentation + available intervals inspired by the corresponding ecto interval implementation
  """

  use Ash.Query.Function, name: :from_now, eager_evaluate?: false

  def args, do: [[:integer, :duration_name]]

  def returns, do: [:utc_datetime]

  def evaluate(%{arguments: [factor, interval]}) do
    now = DateTime.utc_now()
    shifted = Ash.Query.Function.Ago.datetime_add(now, factor, interval)
    {:known, shifted}
  end

  def evaluate(%{arguments: [duration]}) when is_struct(duration, Duration) do
    now = DateTime.utc_now()
    shifted = Ash.Query.Function.Ago.datetime_add(now, duration)
    {:known, shifted}
  end

  def can_return_nil?(_), do: false
end
