defmodule Ash.Query.Function.DateAddTest do
  use ExUnit.Case, async: true

  alias Ash.Query.Function.DateAdd

  describe "date_add query function" do
    test "1 year from today" do
      today = DateTime.utc_now() |> DateTime.to_date()

      assert {:known, %Date{} = date} =
               DateAdd.evaluate(%{arguments: [today, 1, :year]})

      assert date.year == today.year + 1
    end
  end
end
