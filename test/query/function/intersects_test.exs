# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Query.Function.IntersectsTest do
  use ExUnit.Case, async: true

  import Ash.Expr

  alias Ash.Query.Function.Intersects

  test "intersects query function" do
    assert {:known, true} = Intersects.evaluate(%{arguments: [[1, 2, 3], [1]]})
    assert {:known, false} = Intersects.evaluate(%{arguments: [[1, 2, 3], [5]]})
    assert {:known, nil} = Intersects.evaluate(%{arguments: [nil, [5]]})
    assert {:known, nil} = Intersects.evaluate(%{arguments: [[1, 2, 3], nil]})
  end

  test "uses semantic equality, like ==, in and has" do
    assert {:known, true} = Intersects.evaluate(%{arguments: [[1.0], [1]]})
    assert {:known, true} = Intersects.evaluate(%{arguments: [[1], [1.0]]})

    assert {:known, true} =
             Intersects.evaluate(%{arguments: [[Decimal.new("1.0")], [Decimal.new("1")]]})

    assert {:known, false} = Intersects.evaluate(%{arguments: [[1.5], [1]]})
  end

  test "semantic equality in expressions" do
    assert Ash.Expr.eval!(expr(intersects([1.0], [1])))
    assert Ash.Expr.eval!(expr(has([1.0], 1)))
    assert Ash.Expr.eval!(expr(1 in [1.0]))
    refute Ash.Expr.eval!(expr(intersects([1.5], [1])))
  end
end
