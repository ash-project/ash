defmodule Ash.Query.Function.DateTimeAddTest do
  use ExUnit.Case, async: true

  alias Ash.Query.Function.DateTimeAdd

  describe "datetime_add query function" do
    test "1 year from today" do
      today = DateTime.utc_now()

      assert {:known, %DateTime{} = datetime} =
               DateTimeAdd.evaluate(%{arguments: [today, 1, :year]})

      assert datetime.year == today.year + 1
    end
  end
end
