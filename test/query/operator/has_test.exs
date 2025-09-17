defmodule Ash.Query.Operator.HasTest do
  use ExUnit.Case

  alias Ash.Query.Operator.Has

  describe "compare/2" do
    test "returns :mutually_inclusive for equal left and right values" do
      left = %Has{left: [1], right: 1}
      right = %Has{left: [1], right: 1}

      assert Has.compare(left, right) == :mutually_inclusive
    end

    test "returns :mutually_exclusive for different right values" do
      left = %Has{left: [1], right: 2}
      right = %Has{left: [1], right: 1}

      assert Has.compare(left, right) == :mutually_exclusive
    end
  end
end
