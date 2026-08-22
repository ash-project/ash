# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Query.Function.RangeAdjacentTest do
  use ExUnit.Case, async: true

  alias Ash.Query.Function.RangeAdjacent

  defp r(lower, upper, bounds \\ :"[)"),
    do: %Ash.Range{lower: lower, upper: upper, bounds: bounds}

  defp adjacent?(a, b), do: RangeAdjacent.evaluate(%{arguments: [a, b]})

  test "a nil argument is nil" do
    assert {:known, nil} = adjacent?(nil, r(1, 5))
    assert {:known, nil} = adjacent?(r(1, 5), nil)
  end

  test "one ends where the other begins, the seam belonging to exactly one" do
    assert {:known, true} = adjacent?(r(1, 5), r(5, 9))
    assert {:known, true} = adjacent?(r(5, 9), r(1, 5))
    assert {:known, true} = adjacent?(r(1, 5, :"()"), r(5, 9))
  end

  test "sharing the seam or leaving a gap is not adjacency" do
    assert {:known, false} = adjacent?(r(1, 5, :"[]"), r(5, 9))
    assert {:known, false} = adjacent?(r(1, 5, :"()"), r(5, 9, :"()"))
    assert {:known, false} = adjacent?(r(1, 5), r(6, 9))
  end

  test "an empty range is adjacent to nothing" do
    assert {:known, false} = adjacent?(Ash.Range.empty(), r(1, 9))
    assert {:known, false} = adjacent?(Ash.Range.empty(), Ash.Range.empty())
  end
end
