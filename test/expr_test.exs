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
end
