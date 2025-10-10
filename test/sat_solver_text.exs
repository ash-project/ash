defmodule Ash.Test.SatSolver do
  use ExUnit.Case, async: true

  alias Ash.SatSolver

  import Ash.SatSolver, only: [b: 1]

  describe inspect(&SatSolver.simplify_expression/1) do
    test "true and X -> X" do
      assert :x = Ash.SatSolver.simplify_expression(b(true and :x))
    end

    test "X and true -> X" do
      assert :x = Ash.SatSolver.simplify_expression(b(:x and true))
    end

    test "false and X -> false" do
      refute Ash.SatSolver.simplify_expression(b(false and :x))
    end

    test "X and false -> false" do
      refute Ash.SatSolver.simplify_expression(b(:x and false))
    end

    test "true or X -> true" do
      assert Ash.SatSolver.simplify_expression(b(true or :x))
    end

    test "X or true -> true" do
      assert Ash.SatSolver.simplify_expression(b(:x or true))
    end

    test "false or X -> X" do
      assert :x = Ash.SatSolver.simplify_expression(b(false or :x))
    end

    test "X or false -> X" do
      assert :x = Ash.SatSolver.simplify_expression(b(:x or false))
    end

    test "not true -> false" do
      refute Ash.SatSolver.simplify_expression(b(not true))
    end

    test "not false -> true" do
      assert Ash.SatSolver.simplify_expression(b(not false))
    end

    test "not not X -> X" do
      assert :x = Ash.SatSolver.simplify_expression(b(not not :x))
    end

    test "X or not X -> true" do
      assert Ash.SatSolver.simplify_expression(b(:x or not :x))
    end

    test "not X or X -> true" do
      assert Ash.SatSolver.simplify_expression(b(not :x or :x))
    end

    test "X and not X -> false" do
      refute Ash.SatSolver.simplify_expression(b(:x and not :x))
    end

    test "not X and X -> false" do
      refute Ash.SatSolver.simplify_expression(b(not :x and :x))
    end

    test "complex nested expression" do
      # (true and :x) or (false and :y) -> :x or false -> :x
      expr = b((true and :x) or (false and :y))
      assert :x = Ash.SatSolver.simplify_expression(expr)
    end

    test "deeply nested simplification" do
      # ((true or :x) and (false or :y)) -> (true and :y) -> :y
      expr = b((true or :x) and (false or :y))
      assert :y = Ash.SatSolver.simplify_expression(expr)
    end

    test "preserves non-boolean expressions" do
      expr = b({:custom, :check} and :other)
      assert b({:custom, :check} and :other) = Ash.SatSolver.simplify_expression(expr)
    end

    test "left or (left or right) -> left or right" do
      expr = b(:x or (:x or :y))
      assert b(:x or :y) = Ash.SatSolver.simplify_expression(expr)
    end
  end

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

  describe inspect(&SatSolver.expand_expression/3) do
    test "short-circuits on true in or" do
      expr = b(true or :never_evaluated)

      {result, visited} =
        Ash.SatSolver.expand_expression(expr, [], fn node, visited ->
          {node, [node | visited]}
        end)

      assert result
      # Should not visit :never_evaluated
      refute :never_evaluated in visited
    end

    test "short-circuits on false in and" do
      expr = b(false and :never_evaluated)

      {result, visited} =
        Ash.SatSolver.expand_expression(expr, [], fn node, visited ->
          {node, [node | visited]}
        end)

      refute result
      # Should not visit :never_evaluated
      refute :never_evaluated in visited
    end

    test "evaluates both sides when no short-circuit" do
      expr = b(:a and :b)

      {result, visited} =
        Ash.SatSolver.expand_expression(expr, [], fn node, visited ->
          {node, [node | visited]}
        end)

      assert result == expr
      assert :a in visited
      assert :b in visited
    end

    test "simplifies not true immediately" do
      expr = b(not true)

      result = Ash.SatSolver.expand_expression(expr, fn node -> node end)

      refute result
    end

    test "simplifies not false immediately" do
      expr = b(not false)

      result = Ash.SatSolver.expand_expression(expr, fn node -> node end)

      assert result
    end
  end
end
