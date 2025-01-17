defmodule Ash.Query.Function.StartOfDayTest do
  use ExUnit.Case, async: true

  alias Ash.Query.Function.StartOfDay

  test "returns a datetime at the start of the day" do
    assert StartOfDay.evaluate(%{arguments: [Date.utc_today()]}) ==
             {:known, DateTime.to_date(DateTime.utc_now()) |> DateTime.new!(Time.new!(0, 0, 0))}
  end

  test "returns a datetime at the start of the day when given a datetime" do
    assert StartOfDay.evaluate(%{arguments: [DateTime.utc_now()]}) ==
             {:known, DateTime.to_date(DateTime.utc_now()) |> DateTime.new!(Time.new!(0, 0, 0))}
  end
end
