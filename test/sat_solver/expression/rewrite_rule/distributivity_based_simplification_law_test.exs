# SPDX-FileCopyrightText: ash contributors <https://github.com/ash-project/ash/graphs.contributors>
#
#
# SPDX-License-Identifier: MIT

defmodule Ash.Test.SatSolver.Expression.RewriteRule.DistributivityBasedSimplificationLaw do
  use ExUnit.Case, async: true
  use ExUnitProperties

  import Ash.SatSolver.Expression, only: [b: 1]

  alias Ash.SatSolver.Expression
  alias Ash.SatSolver.Expression.RewriteRule.DistributivityBasedSimplificationLaw

  doctest DistributivityBasedSimplificationLaw, import: true

  describe inspect(&DistributivityBasedSimplificationLaw.walk/1) do
    test "applies A AND (NOT A OR B)" do
      assert Expression.postwalk(
               b(:a and (not :a or :b)),
               &DistributivityBasedSimplificationLaw.walk/1
             ) ==
               b(:a and :b)
    end

    test "applies A OR (NOT A AND B)" do
      assert Expression.postwalk(
               b(:a or (not :a and :b)),
               &DistributivityBasedSimplificationLaw.walk/1
             ) ==
               b(:a or :b)
    end

    test "applies NOT A AND (A OR B)" do
      assert Expression.postwalk(
               b(not :a and (:a or :b)),
               &DistributivityBasedSimplificationLaw.walk/1
             ) ==
               b(not :a and :b)
    end

    test "applies NOT A OR (A AND B)" do
      assert Expression.postwalk(
               b(not :a or (:a and :b)),
               &DistributivityBasedSimplificationLaw.walk/1
             ) ==
               b(not :a or :b)
    end

    test "works with complex sub-expressions" do
      expr = b(:x and :y and (not (:x and :y) or :z))

      assert Expression.postwalk(expr, &DistributivityBasedSimplificationLaw.walk/1) ==
               b(:x and :y and :z)
    end

    test "works with deeply nested expressions" do
      expr = b((not :a and :b) or (not (not :a and :b) and (:c or :d)))
      expected = b((not :a and :b) or (:c or :d))
      assert Expression.postwalk(expr, &DistributivityBasedSimplificationLaw.walk/1) == expected
    end

    test "handles multiple variables in patterns" do
      expr = b((:x or :y) and (not (:x or :y) or (:z and :w)))

      assert Expression.postwalk(expr, &DistributivityBasedSimplificationLaw.walk/1) ==
               b((:x or :y) and (:z and :w))
    end

    test "works with boolean constants" do
      expr = b(true and (not true or false))

      assert Expression.postwalk(expr, &DistributivityBasedSimplificationLaw.walk/1) ==
               b(true and false)
    end

    test "handles mixed boolean constants and variables" do
      expr = b(false or (not false and :a))

      assert Expression.postwalk(expr, &DistributivityBasedSimplificationLaw.walk/1) ==
               b(false or :a)
    end

    test "works with complex nested boolean structures" do
      expr = b((not (:a or :b) and :c) or (not (not (:a or :b) and :c) and (:d and :e)))
      expected = b((not (:a or :b) and :c) or (:d and :e))
      assert Expression.postwalk(expr, &DistributivityBasedSimplificationLaw.walk/1) == expected
    end

    test "leaves non-matching patterns unchanged" do
      assert Expression.postwalk(b(:a and :b), &DistributivityBasedSimplificationLaw.walk/1) ==
               b(:a and :b)
    end

    test "leaves different complement patterns unchanged" do
      assert Expression.postwalk(
               b(:a and (not :b or :c)),
               &DistributivityBasedSimplificationLaw.walk/1
             ) ==
               b(:a and (not :b or :c))
    end

    test "leaves single variables unchanged" do
      assert Expression.postwalk(:a, &DistributivityBasedSimplificationLaw.walk/1) == :a
    end

    test "leaves simple negations unchanged" do
      assert Expression.postwalk(b(not :a), &DistributivityBasedSimplificationLaw.walk/1) ==
               b(not :a)
    end

    test "leaves boolean constants unchanged" do
      assert Expression.postwalk(true, &DistributivityBasedSimplificationLaw.walk/1)
      refute Expression.postwalk(false, &DistributivityBasedSimplificationLaw.walk/1)
    end

    test "handles partial distributivity opportunities" do
      expr = b((:a and (not :a or :b)) or (:c and :d))
      result = Expression.postwalk(expr, &DistributivityBasedSimplificationLaw.walk/1)
      assert result == b((:a and :b) or (:c and :d))
    end

    test "works with symmetric patterns" do
      expr = b((:a or (not :a and :b)) and (:c and (not :c or :d)))
      result = Expression.postwalk(expr, &DistributivityBasedSimplificationLaw.walk/1)
      assert result == b((:a or :b) and (:c and :d))
    end

    test "handles multiple distributivity opportunities" do
      expr = b((:a and (not :a or :b)) or (:c or (not :c and :d)))
      result = Expression.postwalk(expr, &DistributivityBasedSimplificationLaw.walk/1)
      assert result == b((:a and :b) or (:c or :d))
    end

    test "preserves expressions without distributivity patterns" do
      expr = b((:a or :b) and (:c or :d))
      assert Expression.postwalk(expr, &DistributivityBasedSimplificationLaw.walk/1) == expr
    end

    test "works with triple nested patterns" do
      expr = b(:x and (not :x or (:y and (not :y or :z))))
      result = Expression.postwalk(expr, &DistributivityBasedSimplificationLaw.walk/1)
      assert result == b(:x and (:y and :z))
    end

    test "handles mixed AND/OR distributivity patterns" do
      expr = b(:a and (not :a or :b) and (:c or (not :c and :d)))
      result = Expression.postwalk(expr, &DistributivityBasedSimplificationLaw.walk/1)
      assert result == b(:a and :b and (:c or :d))
    end

    test "works with very complex identical sub-expressions" do
      complex_expr = b(not (:a or :b) and (:c or not :d))
      expr = b(complex_expr or (not complex_expr and :simple))

      assert Expression.postwalk(expr, &DistributivityBasedSimplificationLaw.walk/1) ==
               b(complex_expr or :simple)
    end

    test "preserves non-distributive complex patterns" do
      expr = b((:a and :b) or (:c and (not :a or :b)))
      assert Expression.postwalk(expr, &DistributivityBasedSimplificationLaw.walk/1) == expr
    end

    test "handles reverse complement patterns" do
      expr = b((not :a or :b) and :a)
      assert Expression.postwalk(expr, &DistributivityBasedSimplificationLaw.walk/1) == expr
    end
  end

  describe "edge cases" do
    test "handles all four patterns in sequence" do
      exprs_and_expected = [
        {b(:x and (not :x or :y)), b(:x and :y)},
        {b(:x or (not :x and :y)), b(:x or :y)},
        {b(not :x and (:x or :y)), b(not :x and :y)},
        {b(not :x or (:x and :y)), b(not :x or :y)}
      ]

      for {expr, expected} <- exprs_and_expected do
        assert Expression.postwalk(expr, &DistributivityBasedSimplificationLaw.walk/1) == expected
      end
    end

    test "works with very complex complement sub-expressions" do
      complex_expr = b(not (:a and :b) or (:c and not :d))
      expr = b(complex_expr and (not complex_expr or :simple))

      assert Expression.postwalk(expr, &DistributivityBasedSimplificationLaw.walk/1) ==
               b(complex_expr and :simple)
    end

    test "preserves non-distributive complex patterns" do
      expr = b(:a and :b and (:c or (not :d and :e)))
      assert Expression.postwalk(expr, &DistributivityBasedSimplificationLaw.walk/1) == expr
    end

    test "handles deeply nested distributive patterns" do
      expr = b(:a and (not :a or (:b and (not :b or :c))))
      expected = b(:a and (:b and :c))
      assert Expression.postwalk(expr, &DistributivityBasedSimplificationLaw.walk/1) == expected
    end

    test "works with multiple levels of distributivity" do
      expr = b(:a or (not :a and (:b or (not :b and :c))))
      result = Expression.postwalk(expr, &DistributivityBasedSimplificationLaw.walk/1)
      assert result == b(:a or (:b or :c))
    end
  end

  property "applying distributivity-based simplification law is idempotent" do
    check all(expr <- Expression.generate_expression(StreamData.atom(:alphanumeric))) do
      result1 = Expression.postwalk(expr, &DistributivityBasedSimplificationLaw.walk/1)
      result2 = Expression.postwalk(result1, &DistributivityBasedSimplificationLaw.walk/1)
      assert result1 == result2
    end
  end

  property "distributivity-based simplification law preserves logical equivalence" do
    check all(
            assignments <-
              StreamData.map_of(StreamData.atom(:alphanumeric), StreamData.boolean(),
                min_length: 1
              ),
            variable_names = Map.keys(assignments),
            expr <- Expression.generate_expression(StreamData.member_of(variable_names))
          ) do
      result = Expression.postwalk(expr, &DistributivityBasedSimplificationLaw.walk/1)
      eval_fn = &Map.fetch!(assignments, &1)

      assert Expression.run(expr, eval_fn) == Expression.run(result, eval_fn),
             """
             Distributivity-based simplification law changed the logical outcome!
             Original: #{inspect(expr, pretty: true)}
             Transformed: #{inspect(result, pretty: true)}
             Assignments: #{inspect(assignments, pretty: true)}
             """
    end
  end
end
