# SPDX-FileCopyrightText: 2020 Zach Daniel
#
# SPDX-License-Identifier: MIT

defmodule Ash.Test.SatSolver.Expression.RewriteRule.AbsorptionLaw do
  use ExUnit.Case, async: true
  use ExUnitProperties

  import Ash.SatSolver.Expression, only: [b: 1]

  alias Ash.SatSolver.Expression
  alias Ash.SatSolver.Expression.RewriteRule.AbsorptionLaw

  doctest AbsorptionLaw, import: true

  describe inspect(&AbsorptionLaw.walk/1) do
    test "applies AND absorption: A AND (A OR B)" do
      assert Expression.postwalk(b(:a and (:a or :b)), &AbsorptionLaw.walk/1) == :a
    end

    test "applies AND absorption: (A OR B) AND A" do
      assert Expression.postwalk(b((:a or :b) and :a), &AbsorptionLaw.walk/1) == :a
    end

    test "applies OR absorption: A OR (A AND B)" do
      assert Expression.postwalk(b(:a or (:a and :b)), &AbsorptionLaw.walk/1) == :a
    end

    test "applies OR absorption: (A AND B) OR A" do
      assert Expression.postwalk(b((:a and :b) or :a), &AbsorptionLaw.walk/1) == :a
    end

    test "works with complex nested expressions" do
      expr = b(:x and :y and ((:x and :y) or :z))
      assert Expression.postwalk(expr, &AbsorptionLaw.walk/1) == b(:x and :y)
    end

    test "works with negated expressions" do
      expr = b(not :a and (not :a or :b))
      assert Expression.postwalk(expr, &AbsorptionLaw.walk/1) == b(not :a)
    end

    test "leaves non-matching patterns unchanged" do
      assert Expression.postwalk(b(:a and :b), &AbsorptionLaw.walk/1) == b(:a and :b)

      assert Expression.postwalk(b(:a and (:b or :c)), &AbsorptionLaw.walk/1) ==
               b(:a and (:b or :c))
    end

    test "handles multiple absorption opportunities" do
      expr = b((:a and (:a or :b)) or (:c or (:c and :d)))
      result = Expression.postwalk(expr, &AbsorptionLaw.walk/1)
      assert result == b(:a or :c)
    end
  end

  describe "edge cases" do
    test "handles reverse absorption patterns" do
      # Test that all four patterns work correctly
      exprs_and_expected = [
        {b(:x and (:x or :y)), :x},
        {b((:x or :y) and :x), :x},
        {b(:x or (:x and :y)), :x},
        {b((:x and :y) or :x), :x}
      ]

      for {expr, expected} <- exprs_and_expected do
        assert Expression.postwalk(expr, &AbsorptionLaw.walk/1) == expected
      end
    end

    test "works with very complex identical sub-expressions" do
      complex_expr = b(not (:a and :b) or (:c and not :d))
      expr = b(complex_expr and (complex_expr or :simple))
      assert Expression.postwalk(expr, &AbsorptionLaw.walk/1) == complex_expr
    end

    test "preserves non-absorbing complex patterns" do
      expr = b((:a and :b) or (:c and (:a or :b)))
      # This doesn't match absorption patterns
      assert Expression.postwalk(expr, &AbsorptionLaw.walk/1) == expr
    end
  end

  property "applying absorption law is idempotent" do
    check all(expr <- Expression.generate_expression(StreamData.atom(:alphanumeric))) do
      result1 = Expression.postwalk(expr, &AbsorptionLaw.walk/1)
      result2 = Expression.postwalk(result1, &AbsorptionLaw.walk/1)
      assert result1 == result2
    end
  end

  property "absorption law preserves logical equivalence" do
    check all(
            assignments <-
              StreamData.map_of(StreamData.atom(:alphanumeric), StreamData.boolean(),
                min_length: 1
              ),
            variable_names = Map.keys(assignments),
            expr <- Expression.generate_expression(StreamData.member_of(variable_names))
          ) do
      result = Expression.postwalk(expr, &AbsorptionLaw.walk/1)
      eval_fn = &Map.fetch!(assignments, &1)

      assert Expression.run(expr, eval_fn) == Expression.run(result, eval_fn),
             """
             Absorption law changed the logical outcome!
             Original: #{inspect(expr, pretty: true)}
             Transformed: #{inspect(result, pretty: true)}
             Assignments: #{inspect(assignments, pretty: true)}
             """
    end
  end
end
