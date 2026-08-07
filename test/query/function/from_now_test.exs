# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Query.Function.FromNowTest do
  use ExUnit.Case, async: true

  require Ash.Expr

  alias Ash.Query.Function.FromNow

  describe "from now query function" do
    test "Years from now" do
      today = Date.utc_today()
      assert {:known, %DateTime{} = datetime} = FromNow.evaluate(%{arguments: [1, :year]})
      assert datetime.year == today.year + 1
    end

    test "Years from now :duration" do
      today = Date.utc_today()

      assert {:known, %DateTime{} = datetime} =
               FromNow.evaluate(%{arguments: [Duration.new!(year: 1)]})

      assert datetime.year == today.year + 1
    end

    # `evaluate/1` is reached only if `args/0` declares the form, so calling it
    # directly does not show whether the function is callable in an expression.
    test "a duration is callable as an expression" do
      today = Date.utc_today()

      assert {:ok, %DateTime{} = datetime} =
               Ash.Expr.eval(Ash.Expr.expr(from_now(^Duration.new!(year: 1))))

      assert datetime.year == today.year + 1
    end
  end
end
