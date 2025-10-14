# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Query.Operator.InTest do
  use ExUnit.Case

  alias Ash.Query.Operator.Eq
  alias Ash.Query.Operator.In

  describe "compare/2" do
    test "returns :mutually_inclusive for equal left and right values" do
      left = %In{left: 1, right: MapSet.new([2, 3])}
      right = %In{left: 1, right: MapSet.new([2, 3])}

      assert In.compare(left, right) == :mutually_inclusive
    end

    test "returns :mutually_exclusive for different right values" do
      left = %In{left: 1, right: MapSet.new([2, 3])}
      right = %In{left: 1, right: MapSet.new([4, 5])}

      assert In.compare(left, right) == :mutually_exclusive
    end

    test "returns :left_includes_right for Eq in In" do
      left = %In{left: 1, right: MapSet.new([2, 3])}
      right = %Eq{left: 1, right: 2}

      assert In.compare(left, right) == :left_includes_right
    end
  end
end
