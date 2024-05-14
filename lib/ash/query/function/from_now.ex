defmodule Ash.Query.Function.FromNow do
  @moduledoc """
  Adds the given interval from the current time in UTC.

  For example:
     expires_at < from_now(7, :day)

  Documentation + available intervals inspired by the corresponding ecto interval implementation
  """

  use Ash.Query.Function, name: :from_now, eager_evaluate?: false

  def args, do: [[:integer, :duration_name]]

  def evaluate(%{arguments: [factor, interval]}) do
    now = DateTime.utc_now()
    shifted = Ash.Query.Function.Ago.datetime_add(now, factor, interval)
    {:known, shifted}
  end

  def can_return_nil?(_), do: false
end
