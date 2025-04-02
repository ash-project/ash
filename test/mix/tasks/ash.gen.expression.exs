defmodule Mix.Tasks.Ash.Gen.CustomExpressionTest do
  use ExUnit.Case
  import Igniter.Test

  @moduletag :igniter

  test "generates a basic custom expression module" do
    test_project()
    |> Igniter.compose_task("ash.gen.custom_expression", [
      "MyApp.Expressions.MyExpression"
    ])
    |> assert_creates("lib/my_app/expressions/my_expression.ex", """
    defmodule MyApp.Expressions.MyExpression do
      use Ash.CustomExpression,
        name: :my_expression,
        args: []

      @impl true
      def expression(_data_layer, args) do
        {:ok, expr(args)}
      end
    end
    """)

    test_project()
    |> Igniter.compose_task("ash.gen.custom_expression", [
      "MyApp.Expressions.MyExpression",
      "--name",
      "my_name",
      "--args",
      "string,atom"
    ])
    |> assert_creates("lib/my_app/expressions/my_expression.ex", """
    defmodule MyApp.Expressions.MyExpression do
      use Ash.CustomExpression,
        name: :my_name,
        args: [:string, :atom]

      @impl true
      def expression(_data_layer, args) do
        {:ok, expr(args)}
      end
    end
    """)
  end
end
