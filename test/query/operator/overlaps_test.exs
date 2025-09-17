defmodule Ash.Query.Operator.OverlapsTest do
  use ExUnit.Case

  alias Ash.Query.Operator.{Has, Overlaps}

  describe "compare/2" do
    test "returns :mutually_inclusive for equal left and right values" do
      left = %Overlaps{left: 1, right: MapSet.new([2, 3])}
      right = %Overlaps{left: 1, right: MapSet.new([2, 3])}

      assert Overlaps.compare(left, right) == :mutually_inclusive
    end

    test "returns :mutually_exclusive for different right values" do
      left = %Overlaps{left: 1, right: MapSet.new([2, 3])}
      right = %Overlaps{left: 1, right: MapSet.new([4, 5])}

      assert Overlaps.compare(left, right) == :mutually_exclusive
    end

    test "returns :left_includes_right for Has in Overlaps" do
      left = %Overlaps{left: 1, right: MapSet.new([2, 3])}
      right = %Has{left: 1, right: 2}

      assert Overlaps.compare(left, right) == :left_includes_right
    end
  end
end
