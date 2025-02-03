defmodule Ash.Test.ExprTest do
  @moduledoc false
  use ExUnit.Case, async: true

  import Ash.Expr

  describe "determine_types" do
    test "it determines the type of an if statement with complex values" do
      {:ok, %func{arguments: args}} =
        expr(
          if fragment("1") do
            string_downcase(type("foo", :string))
          else
            error(Foo, %{bar: "baz"})
          end
        )
        |> Ash.Filter.hydrate_refs(%{})

      determine_types(func, args)
    end
  end

  describe "type coercion" do
    test "integers are coerced to strings" do
      expr = expr("foo" <> type(2024, :string))
      assert eval!(expr) == "foo2024"
    end
  end
end
