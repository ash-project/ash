# SPDX-FileCopyrightText: ash contributors <https://github.com/ash-project/ash/graphs.contributors>
#
#
# SPDX-License-Identifier: MIT

defmodule Ash.Test.SatSolver.Expression.RewriteRule.ComplementLaw do
  use ExUnit.Case, async: true
  use ExUnitProperties

  import Ash.SatSolver.Expression, only: [b: 1]

  alias Ash.SatSolver.Expression
  alias Ash.SatSolver.Expression.RewriteRule.ComplementLaw

  doctest ComplementLaw, import: true

  describe inspect(&ComplementLaw.walk/1) do
    test "applies A OR NOT A = true" do
      assert Expression.postwalk(b(:a or not :a), &ComplementLaw.walk/1)
    end

    test "applies NOT A OR A = true" do
      assert Expression.postwalk(b(not :a or :a), &ComplementLaw.walk/1)
    end

    test "applies A AND NOT A = false" do
      refute Expression.postwalk(b(:a and not :a), &ComplementLaw.walk/1)
    end

    test "applies NOT A AND A = false" do
      refute Expression.postwalk(b(not :a and :a), &ComplementLaw.walk/1)
    end

    test "applies (A AND B) OR (A AND NOT B) = A" do
      expr = b((:a and :b) or (:a and not :b))
      assert Expression.postwalk(expr, &ComplementLaw.walk/1) == :a
    end

    test "applies (A OR B) AND (A OR NOT B) = A" do
      expr = b((:a or :b) and (:a or not :b))
      assert Expression.postwalk(expr, &ComplementLaw.walk/1) == :a
    end

    test "works with complex sub-expressions" do
      expr = b((:x and :y) or not (:x and :y))
      assert Expression.postwalk(expr, &ComplementLaw.walk/1)
    end

    test "works with negated complex expressions" do
      expr = b(not (:a and :b) and (:a and :b))
      refute Expression.postwalk(expr, &ComplementLaw.walk/1)
    end

    test "handles deeply nested complement patterns" do
      expr = b(((:a or :b) and :c) or not ((:a or :b) and :c))
      assert Expression.postwalk(expr, &ComplementLaw.walk/1)
    end

    test "works with boolean constants" do
      assert Expression.postwalk(b(true or not true), &ComplementLaw.walk/1)
      refute Expression.postwalk(b(false and not false), &ComplementLaw.walk/1)
    end

    test "handles mixed boolean constants and variables" do
      assert Expression.postwalk(b((:a and true) or not (:a and true)), &ComplementLaw.walk/1)
    end

    test "works with complement distribution patterns" do
      expr = b((:a and :b and :x) or (:a and :b and not :x))
      assert Expression.postwalk(expr, &ComplementLaw.walk/1) == b(:a and :b)
    end

    test "handles OR distribution with complements" do
      expr = b((:a or :b or :x) and (:a or :b or not :x))
      assert Expression.postwalk(expr, &ComplementLaw.walk/1) == b(:a or :b)
    end

    test "works with multiple variables in complement patterns" do
      expr = b((:x and :y and :z) or not (:x and :y and :z))
      assert Expression.postwalk(expr, &ComplementLaw.walk/1)
    end

    test "handles nested complement distribution" do
      expr = b(((not :a or :b) and :c) or ((not :a or :b) and not :c))
      assert Expression.postwalk(expr, &ComplementLaw.walk/1) == b(not :a or :b)
    end

    test "works with symmetric complement patterns" do
      expr = b((not (:a and :b) or :c) and (not (:a and :b) or not :c))
      assert Expression.postwalk(expr, &ComplementLaw.walk/1) == b(not (:a and :b))
    end

    test "leaves non-complement patterns unchanged" do
      assert Expression.postwalk(b(:a and :b), &ComplementLaw.walk/1) == b(:a and :b)
    end

    test "leaves different variables unchanged" do
      assert Expression.postwalk(b(:a or not :b), &ComplementLaw.walk/1) == b(:a or not :b)
    end

    test "leaves single variables unchanged" do
      assert Expression.postwalk(:a, &ComplementLaw.walk/1) == :a
    end

    test "leaves simple negations unchanged" do
      assert Expression.postwalk(b(not :a), &ComplementLaw.walk/1) == b(not :a)
    end

    test "leaves boolean constants unchanged" do
      assert Expression.postwalk(true, &ComplementLaw.walk/1)
      refute Expression.postwalk(false, &ComplementLaw.walk/1)
    end

    test "handles partial complement opportunities" do
      expr = b((:a or not :a) and (:b and :c))
      result = Expression.postwalk(expr, &ComplementLaw.walk/1)
      assert result == b(true and (:b and :c))
    end

    test "works with mixed complement types" do
      expr = b(:a or not :a or (:b and not :b))
      result = Expression.postwalk(expr, &ComplementLaw.walk/1)
      assert result == b(true or false)
    end

    test "handles multiple complement opportunities" do
      expr = b((:a or not :a) and (:b and not :b))
      result = Expression.postwalk(expr, &ComplementLaw.walk/1)
      assert result == b(true and false)
    end

    test "preserves expressions without complement patterns" do
      expr = b((:a or :b) and (:c or :d))
      assert Expression.postwalk(expr, &ComplementLaw.walk/1) == expr
    end

    test "works with triple complement expressions" do
      expr = b(:x or (not :x or (:x and not :x)))
      result = Expression.postwalk(expr, &ComplementLaw.walk/1)
      assert result == b(:x or (not :x or false))
    end

    test "handles complex distribution patterns" do
      expr = b((not :a and :b and :x) or (not :a and :b and not :x))
      result = Expression.postwalk(expr, &ComplementLaw.walk/1)
      assert result == b(not :a and :b)
    end

    test "works with very complex identical sub-expressions" do
      complex_expr = b(not (:a or :b) and (:c or not :d))
      expr = b(complex_expr or not complex_expr)
      assert Expression.postwalk(expr, &ComplementLaw.walk/1)
    end

    test "preserves non-complement complex patterns" do
      expr = b((:a and :b) or (:c and not :d))
      assert Expression.postwalk(expr, &ComplementLaw.walk/1) == expr
    end
  end

  describe "edge cases" do
    test "handles all complement patterns" do
      exprs_and_expected = [
        {b(:x or not :x), true},
        {b(not :x or :x), true},
        {b(:x and not :x), false},
        {b(not :x and :x), false}
      ]

      for {expr, expected} <- exprs_and_expected do
        assert Expression.postwalk(expr, &ComplementLaw.walk/1) == expected
      end
    end

    test "handles distribution patterns" do
      exprs_and_expected = [
        {b((:x and :y) or (:x and not :y)), :x},
        {b((:x or :y) and (:x or not :y)), :x}
      ]

      for {expr, expected} <- exprs_and_expected do
        assert Expression.postwalk(expr, &ComplementLaw.walk/1) == expected
      end
    end

    test "works with deeply nested complement distribution" do
      expr = b((:a and (:b or :c) and :x) or (:a and (:b or :c) and not :x))
      expected = b(:a and (:b or :c))
      assert Expression.postwalk(expr, &ComplementLaw.walk/1) == expected
    end

    test "preserves non-complement distributive patterns" do
      expr = b((:a and :b) or (:a and :c))
      assert Expression.postwalk(expr, &ComplementLaw.walk/1) == expr
    end

    test "handles mixed complement and non-complement" do
      expr = b((:a or not :a) and (:b or :c))
      result = Expression.postwalk(expr, &ComplementLaw.walk/1)
      assert result == b(true and (:b or :c))
    end
  end

  property "applying complement law is idempotent" do
    check all(expr <- Expression.generate_expression(StreamData.atom(:alphanumeric))) do
      result1 = Expression.postwalk(expr, &ComplementLaw.walk/1)
      result2 = Expression.postwalk(result1, &ComplementLaw.walk/1)
      assert result1 == result2
    end
  end

  property "complement law preserves logical equivalence" do
    check all(
            assignments <-
              StreamData.map_of(StreamData.atom(:alphanumeric), StreamData.boolean(),
                min_length: 1
              ),
            variable_names = Map.keys(assignments),
            expr <- Expression.generate_expression(StreamData.member_of(variable_names))
          ) do
      result = Expression.postwalk(expr, &ComplementLaw.walk/1)
      eval_fn = &Map.fetch!(assignments, &1)

      assert Expression.run(expr, eval_fn) == Expression.run(result, eval_fn),
             """
             Complement law changed the logical outcome!
             Original: #{inspect(expr, pretty: true)}
             Transformed: #{inspect(result, pretty: true)}
             Assignments: #{inspect(assignments, pretty: true)}
             """
    end
  end
end
