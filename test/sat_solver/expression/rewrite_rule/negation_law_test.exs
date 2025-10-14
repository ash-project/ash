# SPDX-FileCopyrightText: 2020 Zach Daniel
#
# SPDX-License-Identifier: MIT

# credo:disable-for-this-file Credo.Check.Warning.BoolOperationOnSameValues
defmodule Ash.Test.SatSolver.Expression.RewriteRule.NegationLaw do
  use ExUnit.Case, async: true
  use ExUnitProperties

  import Ash.SatSolver.Expression, only: [b: 1]

  alias Ash.SatSolver.Expression
  alias Ash.SatSolver.Expression.RewriteRule.NegationLaw

  doctest NegationLaw, import: true

  describe inspect(&NegationLaw.walk/1) do
    test "applies NOT true = false" do
      refute Expression.postwalk(b(not true), &NegationLaw.walk/1)
    end

    test "applies NOT false = true" do
      assert Expression.postwalk(b(not false), &NegationLaw.walk/1)
    end

    test "applies NOT (NOT A) = A" do
      assert Expression.postwalk(b(not not :a), &NegationLaw.walk/1) == :a
    end

    test "eliminates nested double negations" do
      expr = b(not not not not :a)
      result = Expression.postwalk(expr, &NegationLaw.walk/1)
      assert result == :a
    end

    test "works with complex sub-expressions" do
      expr = b(not not (:a and :b))
      result = Expression.postwalk(expr, &NegationLaw.walk/1)
      assert result == b(:a and :b)
    end

    test "leaves single negations unchanged" do
      expr = b(not :a)
      result = Expression.postwalk(expr, &NegationLaw.walk/1)
      assert result == b(not :a)
    end

    test "handles multiple separate double negations" do
      expr = b(not not :a and not not :b)
      result = Expression.postwalk(expr, &NegationLaw.walk/1)
      assert result == b(:a and :b)
    end

    test "works within complex expressions" do
      expr = b((:c or not not :a) and (not not :b or :d))
      result = Expression.postwalk(expr, &NegationLaw.walk/1)
      assert result == b((:c or :a) and (:b or :d))
    end

    test "eliminates quadruple negations" do
      expr = b(not not not not (:x and :y))
      result = Expression.postwalk(expr, &NegationLaw.walk/1)
      assert result == b(:x and :y)
    end

    test "handles mixed double and single negations" do
      expr = b(not :a and not not :b and not not not :c)
      result = Expression.postwalk(expr, &NegationLaw.walk/1)
      assert result == b(not :a and :b and not :c)
    end

    test "works with boolean constants in complex expressions" do
      expr = b(not not true and not not false)
      result = Expression.postwalk(expr, &NegationLaw.walk/1)
      assert result == b(true and false)
    end

    test "handles negated boolean constants" do
      expr = b((not true or not false) and :a)
      result = Expression.postwalk(expr, &NegationLaw.walk/1)
      assert result == b((false or true) and :a)
    end

    test "works with deeply nested negations" do
      expr = b(not (not (not (not (:a or :b)))))
      result = Expression.postwalk(expr, &NegationLaw.walk/1)
      assert result == b(:a or :b)
    end

    test "preserves non-double negation patterns" do
      expr = b(:a or not (:b and not :c))
      result = Expression.postwalk(expr, &NegationLaw.walk/1)
      assert result == b(:a or not (:b and not :c))
    end

    test "handles mixed boolean constant negations" do
      expr = b(not true or (not false and :x))
      result = Expression.postwalk(expr, &NegationLaw.walk/1)
      assert result == b(false or (true and :x))
    end

    test "works with complex nested boolean structures" do
      expr = b(not not ((:a or :b) and not not (:c and :d)))
      result = Expression.postwalk(expr, &NegationLaw.walk/1)
      assert result == b((:a or :b) and (:c and :d))
    end

    test "leaves non-negation patterns unchanged" do
      assert Expression.postwalk(b(:a and :b), &NegationLaw.walk/1) == b(:a and :b)
    end

    test "leaves different variables unchanged" do
      assert Expression.postwalk(b(:a or :b), &NegationLaw.walk/1) == b(:a or :b)
    end

    test "leaves single variables unchanged" do
      assert Expression.postwalk(:a, &NegationLaw.walk/1) == :a
    end

    test "leaves boolean constants unchanged" do
      assert Expression.postwalk(true, &NegationLaw.walk/1)
      refute Expression.postwalk(false, &NegationLaw.walk/1)
    end

    test "handles partial negation opportunities" do
      expr = b(not not :a and (:b or :c))
      result = Expression.postwalk(expr, &NegationLaw.walk/1)
      assert result == b(:a and (:b or :c))
    end

    test "works with symmetric negation patterns" do
      expr = b((not not :a or :b) and (not true or not not :c))
      result = Expression.postwalk(expr, &NegationLaw.walk/1)
      assert result == b((:a or :b) and (false or :c))
    end

    test "handles multiple negation opportunities" do
      expr = b(not not :a or (not false and not not :b))
      result = Expression.postwalk(expr, &NegationLaw.walk/1)
      assert result == b(:a or (true and :b))
    end

    test "preserves expressions without negation patterns" do
      expr = b((:a or :b) and (:c or :d))
      assert Expression.postwalk(expr, &NegationLaw.walk/1) == expr
    end

    test "works with very complex expressions" do
      expr = b(not not (not true or (not not false and :x)))
      result = Expression.postwalk(expr, &NegationLaw.walk/1)
      assert result == b(false or (false and :x))
    end

    test "handles alternating negations" do
      expr = b(not (not (not (not (not :a)))))
      result = Expression.postwalk(expr, &NegationLaw.walk/1)
      assert result == b(not :a)
    end
  end

  describe "edge cases" do
    test "handles all negation patterns" do
      exprs_and_expected = [
        {b(not true), false},
        {b(not false), true},
        {b(not not :x), :x}
      ]

      for {expr, expected} <- exprs_and_expected do
        assert Expression.postwalk(expr, &NegationLaw.walk/1) == expected
      end
    end

    test "works with deeply nested double negations" do
      expr = b(not not (not not (not not :complex_expr)))
      assert Expression.postwalk(expr, &NegationLaw.walk/1) == :complex_expr
    end

    test "preserves single negations in complex patterns" do
      expr = b(not (:a and not (:b or not :c)))
      assert Expression.postwalk(expr, &NegationLaw.walk/1) == expr
    end

    test "handles mixed constant and variable negations" do
      expr = b((not not true and not false) or not not :x)
      result = Expression.postwalk(expr, &NegationLaw.walk/1)
      assert result == b((true and true) or :x)
    end

    test "works with very long negation chains" do
      # 8 consecutive negations should reduce to the original expression
      expr = b(not not not not not not not not :a)
      result = Expression.postwalk(expr, &NegationLaw.walk/1)
      assert result == :a
    end
  end

  property "applying negation law is idempotent" do
    check all(expr <- Expression.generate_expression(StreamData.atom(:alphanumeric))) do
      result1 = Expression.postwalk(expr, &NegationLaw.walk/1)
      result2 = Expression.postwalk(result1, &NegationLaw.walk/1)
      assert result1 == result2
    end
  end

  property "negation law preserves logical equivalence" do
    check all(
            assignments <-
              StreamData.map_of(StreamData.atom(:alphanumeric), StreamData.boolean(),
                min_length: 1
              ),
            variable_names = Map.keys(assignments),
            expr <- Expression.generate_expression(StreamData.member_of(variable_names))
          ) do
      result = Expression.postwalk(expr, &NegationLaw.walk/1)
      eval_fn = &Map.fetch!(assignments, &1)

      assert Expression.run(expr, eval_fn) == Expression.run(result, eval_fn),
             """
             Negation law changed the logical outcome!
             Original: #{inspect(expr, pretty: true)}
             Transformed: #{inspect(result, pretty: true)}
             Assignments: #{inspect(assignments, pretty: true)}
             """
    end
  end
end
