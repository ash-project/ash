defmodule Ash.Test.Type.ArrayTest do
  @moduledoc false
  use ExUnit.Case, async: true

  @default_constraints [
    nil_items?: false,
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
end
