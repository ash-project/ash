defmodule Ash.Test.SatSolver do
  use ExUnit.Case, async: true

  alias Ash.SatSolver

  import Ash.SatSolver, only: [b: 1]

  describe inspect(&SatSolver.walk_expression/3) do
    test "walks all nodes in expression" do
      expr = b((:a and :b) or not :c)

      {result, visited} =
        Ash.SatSolver.walk_expression(expr, [], fn node, visited ->
          {node, [node | visited]}
        end)

      assert result == expr
      # Should visit all nodes including the composed expressions
      assert :a in visited
      assert :b in visited
      assert :c in visited
      assert b(:a and :b) in visited
      assert b(not :c) in visited
      assert expr in visited
    end

    test "transforms nodes during walk" do
      expr = b(:a and :b)

      result =
        Ash.SatSolver.walk_expression(expr, fn
          :a -> :x
          :b -> :y
          other -> other
        end)

      assert b(:x and :y) = result
    end
  end
end
