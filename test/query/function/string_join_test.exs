defmodule Ash.Query.Function.StringJoinTest do
  use ExUnit.Case, async: true

  alias Ash.Query.Function.StringJoin

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
