defmodule Ash.Query.Function.AgoTest do
  use ExUnit.Case, async: true

  alias Ash.Query.Function.Ago

  describe "ago query function" do
    test "Years ago" do
      today = Date.utc_today()
      assert {:known, %DateTime{} = datetime} = Ago.evaluate(%{arguments: [1, :year]})
      assert datetime.year == today.year - 1
    end
  end

  describe "datetime_add helper" do
    test "Add 3 years to 29 Feb" do
      assert %DateTime{year: 2023, month: 3, day: 1} =
               Ago.datetime_add(~U[2020-02-29T00:00:00Z], 3, :year)
    end

    test "Add 4 years to 29 Feb" do
      assert %DateTime{year: 2024, month: 2, day: 29} =
               Ago.datetime_add(~U[2020-02-29T00:00:00Z], 4, :year)
    end

    test "Add 1 month to 30 Jan" do
      assert %DateTime{year: 2020, month: 2, day: 29} =
               Ago.datetime_add(~U[2020-01-30T00:00:00Z], 1, :month)
    end

    test "Add -16 months to 31 Oct" do
      assert %DateTime{year: 2020, month: 6, day: 30} =
               Ago.datetime_add(~U[2021-10-31T00:00:00Z], -16, :month)
    end

    test "Adding -6 months to year 0" do
      assert %DateTime{year: -1, month: 7, day: 4} =
               Ago.datetime_add(~U[0000-01-04T00:00:00Z], -6, :month)
    end

    test "Add 3 weeks to 15 Dec" do
      assert %DateTime{year: 2022, month: 1, day: 5} =
               Ago.datetime_add(~U[2021-12-15T00:00:00Z], 3, :week)
    end

    test "Add -5 days to 05 March" do
      assert %DateTime{year: 2021, month: 2, day: 28} =
               Ago.datetime_add(~U[2021-03-05T00:00:00Z], -5, :day)
    end

    test "Adding 3 hours is the same as adding 180 minutes" do
      now = DateTime.utc_now()
      assert Ago.datetime_add(now, 3, :hour) == Ago.datetime_add(now, 180, :minute)
    end

    test "Adding -5 minutes is the same as adding -300 seconds" do
      now = DateTime.utc_now()
      assert Ago.datetime_add(now, -5, :minute) == DateTime.add(now, -300, :second)
    end
  end
end
