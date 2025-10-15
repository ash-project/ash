# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Query.Function.StringPositionTest do
  use ExUnit.Case, async: true

  import Ash.Expr

  test "string_position is zero based" do
    assert 0 = eval!(expr(string_position("foo-bar", "foo")))
  end

  test "string_position is character based" do
    assert 2 = eval!(expr(string_position("🥳 Woo!", "Woo")))
  end

  test "string_position returns nil if no match" do
    assert {:ok, nil} = eval(expr(string_position("foo", "bar")))
  end

  test "string_position case insensitive string" do
    assert 1 = eval!(expr(string_position(^%Ash.CiString{string: "FOO"}, "oo")))
  end

  test "string_position case insensitive substring" do
    assert 1 = eval!(expr(string_position("FOO", ^%Ash.CiString{string: "oo"})))
  end

  test "string_position both case insensitive" do
    assert 1 =
             eval!(
               expr(string_position(^%Ash.CiString{string: "FOO"}, ^%Ash.CiString{string: "oo"}))
             )
  end
end
