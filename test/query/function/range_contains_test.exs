# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Query.Function.RangeContainsTest do
  use ExUnit.Case, async: true

  alias Ash.Query.Function.RangeContains

  defp r(lower, upper, bounds \\ :"[)"),
    do: %Ash.Range{lower: lower, upper: upper, bounds: bounds}

  defp contains?(range, value), do: RangeContains.evaluate(%{arguments: [range, value]})

  test "a nil argument is nil" do
    assert {:known, nil} = contains?(nil, 5)
    assert {:known, nil} = contains?(r(1, 10), nil)
  end

  test "holds a point between its bounds" do
    assert {:known, true} = contains?(r(1, 10), 5)
    assert {:known, false} = contains?(r(1, 10), 20)
  end

  test "a bound holds its own value only when inclusive" do
    assert {:known, true} = contains?(r(1, 10), 1)
    assert {:known, false} = contains?(r(1, 10), 10)
    assert {:known, true} = contains?(r(1, 10, :"[]"), 10)
    assert {:known, false} = contains?(r(1, 10, :"()"), 1)
  end

  test "holds a range that lies within it" do
    assert {:known, true} = contains?(r(1, 10), r(3, 5))
    assert {:known, true} = contains?(r(1, 10), r(1, 10))
    assert {:known, false} = contains?(r(1, 10), r(5, 20))
  end

  test "an unbounded end holds everything beyond it" do
    assert {:known, true} = contains?(r(nil, 10), -999)
    assert {:known, true} = contains?(r(1, nil), 999)
  end

  test "an empty range holds nothing but the empty range" do
    assert {:known, false} = contains?(Ash.Range.empty(), 5)
    assert {:known, true} = contains?(r(1, 10), Ash.Range.empty())
    assert {:known, true} = contains?(Ash.Range.empty(), Ash.Range.empty())
  end
end
