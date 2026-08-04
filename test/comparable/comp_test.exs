# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule CompTest.Uncomparable do
  @moduledoc false
  defstruct [:value]
end

defmodule CompTest do
  @moduledoc false
  use ExUnit.Case, async: true

  alias CompTest.Uncomparable

  describe "dispatch" do
    # Every pair here answers differently under generic term order, which is
    # case-sensitive, orders a struct before a binary, and orders `Decimal` by
    # its fields rather than its value.
    test "a pair with its own comparator reaches that comparator" do
      assert Comp.compare(Ash.CiString.new("Hello"), "hello") == :eq
      assert Comp.compare("hello", Ash.CiString.new("Hello")) == :eq
      assert Comp.compare(Decimal.new("1.0"), 1) == :eq
      assert Comp.compare(Decimal.new("1.0"), Decimal.new("1")) == :eq
    end

    test "a pair with no comparator falls back to generic term order" do
      assert Comp.compare(Ash.CiString.new("a"), Decimal.new(1)) == :lt
    end

    test "a pair falls back whether or not its module name has been named elsewhere" do
      assert Comp.compare(%Uncomparable{value: 1}, %Uncomparable{value: 2}) == :lt

      Module.concat([Comparable, Type, Uncomparable, To, Uncomparable])

      assert Comp.compare(%Uncomparable{value: 1}, %Uncomparable{value: 2}) == :lt
    end
  end
end
