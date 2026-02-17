defimpl Iter.Iterable, for: Date.Range do
  @moduledoc false

  use Iter.Impl

  @doc false
  @impl true
  def next(date_range)
      when date_range.step > 0 and date_range.first_in_iso_days <= date_range.last_in_iso_days
      when date_range.step < 0 and date_range.first_in_iso_days >= date_range.last_in_iso_days do
    next =
      date_from_iso_days(
        date_range.first_in_iso_days + date_range.step,
        date_range.first.calendar
      )

    next_date_range = Date.range(next, date_range.last, date_range.step)
    {:ok, date_range.first, next_date_range}
  end

  def next(_date_range) do
    :done
  end

  defp date_from_iso_days(days, Calendar.ISO) do
    {year, month, day} = Calendar.ISO.date_from_iso_days(days)
    %Date{year: year, month: month, day: day, calendar: Calendar.ISO}
  end

  defp date_from_iso_days(days, calendar) do
    {year, month, day, _, _, _, _} =
      calendar.naive_datetime_from_iso_days({days, {0, 86_400_000_000}})

    %Date{year: year, month: month, day: day, calendar: calendar}
  end

  @doc false
  @impl true
  def count(date_range) do
    {:ok, count} = Enumerable.count(date_range)
    count
  end

  @doc false
  @impl true
  def member?(date_range, element) do
    case Enumerable.member?(date_range, element) do
      {:ok, true} -> true
      _ -> false
    end
  end
end
