defmodule Ash.Query.Function.NowTest do
  use ExUnit.Case, async: true

  alias Ash.Query.Function.Now

  describe "now query function" do
    test "now/0" do
      assert {:known, %DateTime{}} =
               Now.evaluate(%{arguments: []})
    end

    test "now/1" do
      assert {:known, %DateTime{}} =
               Now.evaluate(%{arguments: ["Etc/UTC"]})
    end

    test "now/2" do
      assert {:known, %DateTime{}} =
               Now.evaluate(%{arguments: ["Etc/UTC", Calendar.get_time_zone_database()]})
    end
  end
end
