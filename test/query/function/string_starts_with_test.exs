# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Query.Function.StringStartsWithTest do
  use ExUnit.Case, async: true

  import Ash.Expr

  test "returns true when string starts with the prefix" do
    assert eval!(expr(string_starts_with("foobar", "foo")))
  end

  test "returns false when string does not start with the prefix" do
    refute eval!(expr(string_starts_with("foobar", "bar")))
  end

  test "case-insensitive on the left" do
    assert eval!(expr(string_starts_with(^%Ash.CiString{string: "FooBar"}, "foo")))
  end

  test "case-insensitive on the right" do
    assert eval!(expr(string_starts_with("FooBar", ^%Ash.CiString{string: "foo"})))
  end

  test "false on nil" do
    refute eval!(expr(string_starts_with(nil, "foo")))
    refute eval!(expr(string_starts_with("foo", nil)))
  end
end
