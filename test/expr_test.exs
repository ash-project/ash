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

  describe "string interpolation" do
    test "pinned values can be used in interpolation" do
      var = "foo"
      expr = expr("#{^var}-#{^var}")
      assert eval!(expr) == "foo-foo"
    end
  end

  describe "type coercion" do
    test "integers are coerced to strings" do
      expr = expr("foo" <> type(2024, :string))
      assert eval!(expr) == "foo2024"
    end
  end

  describe "rem expressions" do
    test "evaluates" do
      expr = expr(rem(1, 2) == 0)
      assert eval!(expr) == false
      expr = expr(rem(2, 2) == 0)
      assert eval!(expr) == true
    end
  end

  describe "case expressions" do
    test "raises error when using case expressions" do
      assert_raise ArgumentError, ~r/`case` expressions are not supported/, fn ->
        Code.eval_quoted(
          quote do
            import Ash.Expr

            expr(
              case :role do
                :principal -> 1
                :teacher -> 2
                :student -> 3
              end
            )
          end
        )
      end
    end
  end
end
