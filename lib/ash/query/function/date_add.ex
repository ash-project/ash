defmodule Ash.Query.Function.DateAdd do
  @moduledoc """
  Adds the given interval to the current time in UTC

  For example:
     activates_at < date_add(today(), 7, :day)

  Documentation + available intervals inspired by the corresponding ecto interval implementation
  """

  use Ash.Query.Function, name: :date_add, eager_evaluate?: false

  @beginning_of_day Time.new!(0, 0, 0)

  def args, do: [[:date, :integer, :duration_name]]

  def returns, do: [:date]

  def evaluate(%{arguments: [date, factor, interval]}) do
    with {:ok, datetime} <- DateTime.new(date, @beginning_of_day),
         shifted <- Ash.Query.Function.Ago.datetime_add(datetime, factor, interval),
         truncated <- DateTime.to_date(shifted) do
      {:known, truncated}
    end
  end

  def can_return_nil?(%{arguments: [date | _]}) do
    Ash.Expr.can_return_nil?(date)
  end
end
