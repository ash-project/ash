# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Query.Function.StringEndsWithTest do
  use ExUnit.Case, async: true

  import Ash.Expr

  test "returns true when string ends with the suffix" do
    assert eval!(expr(string_ends_with("foobar", "bar")))
  end

  test "returns false when string does not end with the suffix" do
    refute eval!(expr(string_ends_with("foobar", "foo")))
  end

  test "case-insensitive on the left" do
    assert eval!(expr(string_ends_with(^%Ash.CiString{string: "FooBar"}, "BAR")))
  end

  test "case-insensitive on the right" do
    assert eval!(expr(string_ends_with("FooBar", ^%Ash.CiString{string: "BAR"})))
  end

  test "false on nil" do
    refute eval!(expr(string_ends_with(nil, "bar")))
    refute eval!(expr(string_ends_with("foo", nil)))
  end
end
