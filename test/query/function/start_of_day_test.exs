# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs.contributors>
#
# SPDX-License-Identifier: MIT

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

  @nz_tz "Pacific/Auckland"

  test "timezone handled correctly" do
    utc = ~U[2025-04-09 19:00:00Z]
    assert DateTime.to_date(utc) == ~D[2025-04-09]
    nz = DateTime.shift_zone!(utc, @nz_tz)
    # The 10th in New Zealand!
    assert DateTime.to_date(nz) == ~D[2025-04-10]
    expected_start_of_day_nz = DateTime.new!(~D[2025-04-10], ~T[00:00:00], @nz_tz)
    {:known, start_of_day_utc} = StartOfDay.evaluate(%{arguments: [utc, @nz_tz]})

    assert DateTime.shift_zone!(start_of_day_utc, @nz_tz) ==
             expected_start_of_day_nz
  end
end
