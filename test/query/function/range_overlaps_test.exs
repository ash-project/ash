# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Query.Function.RangeOverlapsTest do
  use ExUnit.Case, async: true

  alias Ash.Query.Function.RangeOverlaps

  defp r(lower, upper, bounds \\ :"[)"), do: %Ash.Range{lower: lower, upper: upper, bounds: bounds}
  defp overlap?(a, b), do: RangeOverlaps.evaluate(%{arguments: [a, b]})

  test "nil argument is unknown/nil" do
    assert {:known, nil} = overlap?(nil, r(1, 5))
    assert {:known, nil} = overlap?(r(1, 5), nil)
  end

  test "interiors that overlap" do
    assert {:known, true} = overlap?(r(1, 10), r(5, 15))
    assert {:known, true} = overlap?(r(5, 15), r(1, 10))
    assert {:known, true} = overlap?(r(1, 10), r(2, 3))
  end

  test "disjoint with a gap do not overlap" do
    assert {:known, false} = overlap?(r(1, 5), r(6, 10))
  end

  describe "touching at a boundary depends on inclusivity" do
    test "half-open adjacent ranges do NOT overlap ([)+[) — the temporal case)" do
      assert {:known, false} = overlap?(r(1, 5, :"[)"), r(5, 10, :"[)"))
    end

    test "inclusive upper meeting inclusive lower DO overlap (share the point)" do
      assert {:known, true} = overlap?(r(1, 5, :"[]"), r(5, 10, :"[)"))
      assert {:known, true} = overlap?(r(1, 5, :"(]"), r(5, 10, :"[]"))
    end

    test "an exclusive bound on either side at the seam means no shared point" do
      assert {:known, false} = overlap?(r(1, 5, :"[]"), r(5, 10, :"()"))
      assert {:known, false} = overlap?(r(1, 5, :"[)"), r(5, 10, :"[]"))
    end
  end

  describe "unbounded (nil) ends are ±∞" do
    test "overlap through open ends" do
      assert {:known, true} = overlap?(r(nil, 5), r(3, nil))
      assert {:known, true} = overlap?(r(nil, nil), r(100, 200))
    end

    test "still disjoint when the bounded ends don't meet" do
      assert {:known, false} = overlap?(r(nil, 5), r(5, nil, :"[)"))
    end
  end

  describe "empty ranges overlap nothing" do
    test "[5,5) is empty" do
      assert {:known, false} = overlap?(r(5, 5, :"[)"), r(1, 10))
      assert {:known, false} = overlap?(r(1, 10), r(5, 5, :"()"))
    end

    test "[5,5] is the single point 5 (not empty), so it can overlap" do
      assert {:known, true} = overlap?(r(5, 5, :"[]"), r(1, 10, :"[)"))
      assert {:known, false} = overlap?(r(5, 5, :"[]"), r(6, 10, :"[)"))
    end

    test "lower > upper is empty" do
      assert {:known, false} = overlap?(r(10, 1), r(1, 100))
    end
  end
end
