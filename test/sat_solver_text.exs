# SPDX-FileCopyrightText: 2020 Zach Daniel
#
# SPDX-License-Identifier: MIT

# credo:disable-for-this-file Credo.Check.Warning.BoolOperationOnSameValues
defmodule Ash.Test.SatSolver do
  use ExUnit.Case, async: true
  use ExUnitProperties

  alias Ash.SatSolver

  import Ash.SatSolver, only: [b: 1]

  describe inspect(&SatSolver.simplify_expression/1) do
    test "Identity Law (AND): true and X -> X" do
      assert :x = Ash.SatSolver.simplify_expression(b(true and :x))
    end

    test "Identity Law (AND): X and true -> X" do
      assert :x = Ash.SatSolver.simplify_expression(b(:x and true))
    end

    test "Annihilator Law (AND): false and X -> false" do
      refute Ash.SatSolver.simplify_expression(b(false and :x))
    end

    test "Annihilator Law (AND): X and false -> false" do
      refute Ash.SatSolver.simplify_expression(b(:x and false))
    end

    test "Annihilator Law (OR): true or X -> true" do
      assert Ash.SatSolver.simplify_expression(b(true or :x))
    end

    test "Annihilator Law (OR): X or true -> true" do
      assert Ash.SatSolver.simplify_expression(b(:x or true))
    end

    test "Identity Law (OR): false or X -> X" do
      assert :x = Ash.SatSolver.simplify_expression(b(false or :x))
    end

    test "Identity Law (OR): X or false -> X" do
      assert :x = Ash.SatSolver.simplify_expression(b(:x or false))
    end

    test "Negation Law: not true -> false" do
      refute Ash.SatSolver.simplify_expression(b(not true))
    end

    test "Negation Law: not false -> true" do
      assert Ash.SatSolver.simplify_expression(b(not false))
    end

    test "Double Negation Law: not not X -> X" do
      assert :x = Ash.SatSolver.simplify_expression(b(not not :x))
    end

    test "Complement Law (OR): X or not X -> true" do
      assert Ash.SatSolver.simplify_expression(b(:x or not :x))
    end

    test "Complement Law (OR): not X or X -> true" do
      assert Ash.SatSolver.simplify_expression(b(not :x or :x))
    end

    test "Complement Law (AND): X and not X -> false" do
      refute Ash.SatSolver.simplify_expression(b(:x and not :x))
    end

    test "Complement Law (AND): not X and X -> false" do
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

    test "Associativity (partial): X or (X or Y) -> X or Y" do
      expr = b(:x or (:x or :y))
      assert b(:x or :y) = Ash.SatSolver.simplify_expression(expr)
    end

    # Idempotent Laws
    test "Idempotent Law: X and X -> X" do
      expr = b(:x and :x)
      assert :x = Ash.SatSolver.simplify_expression(expr)
    end

    test "Idempotent Law: X or X -> X" do
      expr = b(:x or :x)
      assert :x = Ash.SatSolver.simplify_expression(expr)
    end

    # Absorption Laws
    test "Absorption Law: X or (X and Y) -> X" do
      expr = b(:x or (:x and :y))
      assert :x = Ash.SatSolver.simplify_expression(expr)
    end

    test "Absorption Law: X and (X or Y) -> X" do
      expr = b(:x and (:x or :y))
      assert :x = Ash.SatSolver.simplify_expression(expr)
    end

    test "Absorption Law: (X and Y) or X -> X" do
      expr = b((:x and :y) or :x)
      assert :x = Ash.SatSolver.simplify_expression(expr)
    end

    test "Absorption Law: (X or Y) and X -> X" do
      expr = b((:x or :y) and :x)
      assert :x = Ash.SatSolver.simplify_expression(expr)
    end

    # Associativity Optimizations
    test "Associativity: X and (X and Y) -> X and Y" do
      expr = b(:x and (:x and :y))
      assert b(:x and :y) = Ash.SatSolver.simplify_expression(expr)
    end

    test "Associativity: (X and Y) and X -> X and Y" do
      expr = b(:x and :y and :x)
      assert b(:x and :y) = Ash.SatSolver.simplify_expression(expr)
    end

    test "Associativity: (X or Y) or X -> X or Y" do
      expr = b(:x or :y or :x)
      assert b(:x or :y) = Ash.SatSolver.simplify_expression(expr)
    end

    # Distributivity-based Simplifications
    test "Distributivity: X and (not X or Y) -> X and Y" do
      expr = b(:x and (not :x or :y))
      assert b(:x and :y) = Ash.SatSolver.simplify_expression(expr)
    end

    test "Distributivity: X or (not X and Y) -> X or Y" do
      expr = b(:x or (not :x and :y))
      assert b(:x or :y) = Ash.SatSolver.simplify_expression(expr)
    end

    test "Distributivity: not X and (X or Y) -> not X and Y" do
      expr = b(not :x and (:x or :y))
      assert b(not :x and :y) = Ash.SatSolver.simplify_expression(expr)
    end

    test "Distributivity: not X or (X and Y) -> not X or Y" do
      expr = b(not :x or (:x and :y))
      assert b(not :x or :y) = Ash.SatSolver.simplify_expression(expr)
    end

    # Additional Complement Laws
    test "Complement Law: (X and Y) or (X and not Y) -> X" do
      expr = b((:x and :y) or (:x and not :y))
      assert :x = Ash.SatSolver.simplify_expression(expr)
    end

    test "Complement Law: (X or Y) and (X or not Y) -> X" do
      expr = b((:x or :y) and (:x or not :y))
      assert :x = Ash.SatSolver.simplify_expression(expr)
    end

    # Complex nested expressions testing multiple laws
    test "Complex: Nested idempotent and absorption" do
      # (x and x) or ((x and x) and y) -> x or (x and y) -> x
      expr = b((:x and :x) or (:x and :x and :y))
      assert :x = Ash.SatSolver.simplify_expression(expr)
    end

    test "Complex: Multiple distributivity simplifications" do
      # (x or (not x and y)) and (z or (not z and w))
      # -> (x or y) and (z or w)
      expr = b((:x or (not :x and :y)) and (:z or (not :z and :w)))
      assert b((:x or :y) and (:z or :w)) = Ash.SatSolver.simplify_expression(expr)
    end

    test "Complex: Chained associativity" do
      # ((x and y) and x) and z -> (x and y) and z
      expr = b(:x and :y and :x and :z)
      assert b(:x and :y and :z) = Ash.SatSolver.simplify_expression(expr)
    end

    test "Complex: Mixed complement and absorption" do
      # (x or not x) and (x or (x and y)) -> true and x -> x
      expr = b((:x or not :x) and (:x or (:x and :y)))
      assert :x = Ash.SatSolver.simplify_expression(expr)
    end

    property "idempotent simplification" do
      check all(expr <- Ash.SatSolver.generate_expression(StreamData.atom(:alphanumeric))) do
        simplified = Ash.SatSolver.simplify_expression(expr)
        re_simplified = Ash.SatSolver.simplify_expression(simplified)
        assert simplified == re_simplified
      end
    end

    property "simplified gets same result as SAT solver" do
      check all(
              assignments <-
                StreamData.map_of(StreamData.atom(:alphanumeric), StreamData.boolean(),
                  min_length: 1
                ),
              variable_names = Enum.map(assignments, &elem(&1, 0)),
              expr <- Ash.SatSolver.generate_expression(StreamData.member_of(variable_names))
            ) do
        simplified = Ash.SatSolver.simplify_expression(expr)

        assert run_test_expression(expr, assignments) ==
                 run_test_expression(simplified, assignments),
               """
               Simplification changed the logical outcome!
               Original: #{inspect(expr, pretty: true)}
               Simplified: #{inspect(simplified, pretty: true)}
               Result: #{inspect(run_test_expression(expr, assignments), pretty: true)}
               Result after simplification: #{inspect(run_test_expression(simplified, assignments), pretty: true)}
               Assignments: #{inspect(assignments, pretty: true)}
               """
      end
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

  @spec run_test_expression(
          expression :: Ash.SatSolver.boolean_expression(),
          assignments :: %{atom() => boolean()}
        ) :: boolean()
  defp run_test_expression(expression, assignments)

  defp run_test_expression(b(left and right), assignments),
    do: run_test_expression(left, assignments) and run_test_expression(right, assignments)

  defp run_test_expression(b(left or right), assignments),
    do: run_test_expression(left, assignments) or run_test_expression(right, assignments)

  defp run_test_expression(b(not expression), assignments),
    do: not run_test_expression(expression, assignments)

  defp run_test_expression(boolean, _) when is_boolean(boolean), do: boolean

  defp run_test_expression(atom, assignments) when is_atom(atom),
    do: Map.fetch!(assignments, atom)
end
