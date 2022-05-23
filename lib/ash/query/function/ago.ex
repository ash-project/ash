defmodule Ash.Query.Function.Ago do
  @moduledoc """
  Subtracts the given interval from the current time in UTC.

  For example:
     deleted_at > ago(7, :days)

  Documentation + available intervals inspired by the corresponding ecto interval implementation
  """
  use Ash.Query.Function, name: :ago

  def args, do: [[:integer, :duration_name]]

  def evaluate(%{arguments: [factor, interval]}) do
    interval =
      case interval do
        :year -> :years
        :month -> :months
        :week -> :weeks
        :day -> :days
        :hour -> :hours
        :minute -> :minutes
        :second -> :seconds
        :millisecond -> :milliseconds
        :microsecond -> :microseconds
      end

    {:known, Timex.shift(Timex.now(), [{interval, factor * -1}])}
  end
end
