# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Query.Function.RangeBoundsTest do
  use ExUnit.Case, async: true

  alias Ash.Query.Function.RangeLower
  alias Ash.Query.Function.RangeUpper

  defp r(lower, upper, bounds \\ :"[)"),
    do: %Ash.Range{lower: lower, upper: upper, bounds: bounds}

  defp lower(range), do: RangeLower.evaluate(%{arguments: [range]})
  defp upper(range), do: RangeUpper.evaluate(%{arguments: [range]})

  test "a nil range is nil" do
    assert {:known, nil} = lower(nil)
    assert {:known, nil} = upper(nil)
  end

  test "each end is returned as the inner type holds it" do
    assert {:known, 1} = lower(r(1, 10))
    assert {:known, 10} = upper(r(1, 10))
  end

  test "the value is returned whatever the inclusivity" do
    assert {:known, 1} = lower(r(1, 10, :"(]"))
    assert {:known, 10} = upper(r(1, 10, :"[]"))
  end

  test "an unbounded end is nil" do
    assert {:known, nil} = lower(r(nil, 10))
    assert {:known, 10} = upper(r(nil, 10))
    assert {:known, 1} = lower(r(1, nil))
    assert {:known, nil} = upper(r(1, nil))
  end

  test "an empty range has neither end" do
    assert {:known, nil} = lower(Ash.Range.empty())
    assert {:known, nil} = upper(Ash.Range.empty())
  end

  test "a datetime range keeps its inner type" do
    from = ~U[2026-01-01 00:00:00.000000Z]
    to = ~U[2026-02-01 00:00:00.000000Z]

    assert {:known, ^from} = lower(r(from, to))
    assert {:known, ^to} = upper(r(from, to))
  end
end
