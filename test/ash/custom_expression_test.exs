defmodule Ash.CustomExpressionTest do
  use ExUnit.Case, async: false

  import Ash.Expr

  test "custom expressions are callable and evaluate" do
    assert eval!(expr(jaro_distance("foo", "bar"))) == String.jaro_distance("foo", "bar")
  end
end
