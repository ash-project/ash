defmodule Ash.Query.Function.StringSplitTest do
  use ExUnit.Case, async: true

  import Ash.Expr

  test "strings are split on the separator" do
    assert ["fo", "ar"] = eval!(expr(string_split("foo-bar", "o-b")))
  end

  test "strings are split on characters if a separator is not set" do
    assert ["a", "b"] = eval!(expr(string_split("a b")))
  end

  test "splits are not trimmed by default" do
    assert ["", "a", "b", ""] = eval!(expr(string_split("ab", "")))
  end

  test "splits can be trimmed on request" do
    assert ["", "a", "b", ""] = eval!(expr(string_split("ab", "", split?: true)))
  end
end
