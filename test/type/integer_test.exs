# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Test.Type.IntegerTest do
  @moduledoc false
  use ExUnit.Case, async: true

  require Ash.Query

  describe "atomic validation" do
    test "valid value" do
      {:atomic, expr} = Ash.Type.Integer.cast_atomic(1, min: 0, max: 2)

      assert Ash.Expr.eval!(expr) == 1
    end

    test "invalid values" do
      assert_raise Ash.Error.Changes.InvalidChanges, ~r/must be greater than or equal to 2/, fn ->
        {:ok, expr} = Ash.Type.Integer.apply_atomic_constraints(1, min: 2, max: 4)
        Ash.Expr.eval!(expr)
      end

      assert_raise Ash.Error.Changes.InvalidChanges, ~r/must be less than or equal to 4/, fn ->
        {:ok, expr} = Ash.Type.Integer.apply_atomic_constraints(5, min: 2, max: 4)
        Ash.Expr.eval!(expr)
      end
    end
  end

  describe "validation" do
    test "valid value" do
      assert {:ok, 1} = Ash.Type.Integer.cast_input(1, min: 0, max: 2)
    end

    test "invalid values" do
      assert {:error, [[message: "must be more than or equal to %{min}", min: 2]]} =
               Ash.Type.Integer.apply_constraints(1, min: 2, max: 4)

      assert {:error, [[message: "must be less than or equal to %{max}", max: 4]]} =
               Ash.Type.Integer.apply_constraints(5, min: 2, max: 4)
    end
  end
end
