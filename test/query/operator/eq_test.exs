# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Query.Operator.EqTest do
  use ExUnit.Case

  alias Ash.Query.Operator.Eq
  alias Ash.Query.Operator.In

  describe "compare/2" do
    test "returns :mutually_inclusive for equal left and right values" do
      left = %Eq{left: 1, right: 2}
      right = %Eq{left: 1, right: 2}

      assert Eq.compare(left, right) == :mutually_inclusive
    end

    test "returns :mutually_exclusive for different right values" do
      left = %Eq{left: 1, right: 2}
      right = %Eq{left: 1, right: 3}

      assert Eq.compare(left, right) == :mutually_exclusive
    end

    test "returns :right_includes_left for Eq in In" do
      left = %Eq{left: 1, right: 2}
      right = %In{left: 1, right: MapSet.new([2, 3])}

      assert Eq.compare(left, right) == :right_includes_left
    end
  end
end
