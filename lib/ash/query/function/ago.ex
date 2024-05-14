defmodule Ash.Query.Function.Ago do
  @moduledoc """
  Subtracts the given interval from the current time in UTC.

  For example:
     deleted_at > ago(7, :day)

  Documentation + available intervals inspired by the corresponding ecto interval implementation
  """

  use Ash.Query.Function, name: :ago, eager_evaluate?: false

  def args, do: [[:integer, :duration_name]]

  def evaluate(%{arguments: [factor, interval]}) do
    now = DateTime.utc_now()
    shifted = datetime_add(now, -factor, interval)
    {:known, shifted}
  end

  @doc false
  def datetime_add(datetime, amount_to_add, unit)
      when unit in [:microsecond, :millisecond, :second] do
    DateTime.add(datetime, amount_to_add, unit)
  end

  def datetime_add(datetime, amount_to_add, :minute) do
    DateTime.add(datetime, amount_to_add * 60, :second)
  end

  def datetime_add(datetime, amount_to_add, :hour) do
    DateTime.add(datetime, amount_to_add * 60 * 60, :second)
  end

  def datetime_add(datetime, amount_to_add, :day) do
    date = Date.add(datetime, amount_to_add)
    %{datetime | year: date.year, month: date.month, day: date.day}
  end

  def datetime_add(datetime, amount_to_add, :week) do
    datetime_add(datetime, 7 * amount_to_add, :day)
  end

  def datetime_add(datetime, amount_to_add, :month) do
    months_since_zero = datetime.year * 12 + datetime.month - 1 + amount_to_add
    year = div(months_since_zero, 12)

    {year, month} =
      case rem(months_since_zero, 12) do
        x when x < 0 -> {year - 1, 12 + x + 1}
        x -> {year, x + 1}
      end

    # clamp to last day of month
    last_day = Calendar.ISO.days_in_month(year, month)
    day = min(datetime.day, last_day)
    %{datetime | year: year, month: month, day: day}
  end

  def datetime_add(%{month: month, day: day} = datetime, amount_to_add, :year) do
    result = %{datetime | year: datetime.year + amount_to_add}
    # Adjust 29-Feb to 01-March when not a leap year
    if month == 2 and day == 29 and not Calendar.ISO.leap_year?(result.year) do
      %{result | month: 3, day: 1}
    else
      result
    end
  end

  def can_return_nil?(_), do: false
end
