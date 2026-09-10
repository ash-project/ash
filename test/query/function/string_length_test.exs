# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Test.Query.Function.StringLengthTest do
  @moduledoc false
  use ExUnit.Case, async: true

  import Ash.Expr

  @combining "a" <> String.duplicate("\u0301", 10)

  test "counts using the configured default (codepoints in the test config)" do
    assert eval!(expr(string_length(^@combining))) == 11
    assert eval!(expr(string_length(^Ash.CiString.new(@combining)))) == 11
  end

  test "counts graphemes, codepoints or bytes when asked" do
    assert eval!(expr(string_length(^@combining, :graphemes))) == 1
    assert eval!(expr(string_length(^@combining, :codepoints))) == 11
    # "a" is one byte, each U+0301 is two
    assert eval!(expr(string_length(^@combining, :bytes))) == 21

    assert eval!(expr(string_length(^Ash.CiString.new(@combining), :codepoints))) == 11
  end

  test "rejects unknown counts" do
    assert {:error, error} = Ash.Expr.eval(expr(string_length(^@combining, :words)))
    assert inspect(error) =~ "Invalid unit :words"
  end
end
