# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Test.Type.ArrayTest do
  @moduledoc false
  use ExUnit.Case, async: true

  @default_constraints [
    nil_items?: false,
    remove_nil_items?: false,
    empty_values: [""]
  ]

  test "it errors when containing a nil value" do
    assert {:error, [[message: "no nil values", index: 1]]} =
             Ash.Type.apply_constraints(
               {:array, :string},
               ["something", nil],
               @default_constraints
             )
  end

  test "it errors when containing an empty string (which is converted to nil by the string type)" do
    assert {:error, [[message: "no nil values", index: 1]]} =
             Ash.Type.apply_constraints(
               {:array, :string},
               ["something", ""],
               @default_constraints
             )
  end

  test "it removes nil values instead of erroring with remove_nil_items?: true" do
    assert {:ok, ["something", "something else"]} =
             Ash.Type.apply_constraints(
               {:array, :string},
               ["something", nil, nil, "something else"],
               Keyword.put(@default_constraints, :remove_nil_items?, true)
             )
  end

  test "it errors when array length is below min_length constraint" do
    assert {:error, [message: "must have %{min} or more items", min: 1]} =
             Ash.Type.apply_constraints(
               {:array, :string},
               [""],
               Keyword.merge(@default_constraints,
                 min_length: 1,
                 remove_nil_items?: true
               )
             )
  end
end
