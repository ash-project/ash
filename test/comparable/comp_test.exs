# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule CompTest do
  @moduledoc false
  use ExUnit.Case, async: true

  describe "dispatch" do
    test "a pair with its own comparator reaches that comparator" do
      # Both of these are answered by a `defcomparable` for the pair, and both
      # would answer differently under the generic fallback: Erlang term order
      # is case-sensitive and orders any struct after any binary, and orders
      # `Decimal` structs by their fields rather than by their value.
      assert Comp.compare(Ash.CiString.new("Hello"), "hello") == :eq
      assert Comp.compare("hello", Ash.CiString.new("Hello")) == :eq
      assert Comp.compare(Decimal.new("1.0"), 1) == :eq
      assert Comp.compare(Decimal.new("1.0"), Decimal.new("1")) == :eq
    end

    test "a pair with no comparator falls back to generic term order" do
      assert Comp.compare(Ash.CiString.new("a"), Decimal.new(1)) == :lt
    end
  end
end
