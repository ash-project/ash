defmodule Ash.Query.Function.StringJoinTest do
  use ExUnit.Case, async: true

  alias Ash.Query.Function.StringJoin
  import Ash.CiString, only: [sigil_i: 2]
  alias Ash.CiString

  describe "string" do
    test "join strings with separator" do
      assert {:known, "one, two"} = StringJoin.evaluate(%{arguments: [["one", "two"], ", "]})
      assert {:known, "one"} = StringJoin.evaluate(%{arguments: [["one", nil], ", "]})
      assert {:known, ""} = StringJoin.evaluate(%{arguments: [[nil, nil], ", "]})
    end

    test "join strings without separator" do
      assert {:known, "onetwo"} = StringJoin.evaluate(%{arguments: [["one", "two"]]})
      assert {:known, "one"} = StringJoin.evaluate(%{arguments: [["one", nil]]})
      assert {:known, ""} = StringJoin.evaluate(%{arguments: [[nil, nil]]})
    end
  end

  describe "ci_string" do
    test "join strings with separator" do
      assert {:known, %CiString{string: "one, two"}} =
               StringJoin.evaluate(%{arguments: [[~i"one", ~i"two"], ~i", "]})

      assert {:known, %CiString{string: "one"}} =
               StringJoin.evaluate(%{arguments: [[~i"one", nil], ~i", "]})

      assert {:known, %CiString{string: ""}} =
               StringJoin.evaluate(%{arguments: [[nil, nil], ~i", "]})
    end

    test "join strings without separator" do
      assert {:known, %CiString{string: "onetwo"}} =
               StringJoin.evaluate(%{arguments: [[~i"one", ~i"two"]]})

      assert {:known, %CiString{string: "one"}} =
               StringJoin.evaluate(%{arguments: [[~i"one", nil]]})

      assert {:known, %CiString{string: ""}} = StringJoin.evaluate(%{arguments: [[nil, nil]]})
    end
  end
end
