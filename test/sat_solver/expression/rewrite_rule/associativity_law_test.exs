# SPDX-FileCopyrightText: 2020 Zach Daniel
#
# SPDX-License-Identifier: MIT

defmodule Ash.Test.SatSolver.Expression.RewriteRule.AssociativityLaw do
  use ExUnit.Case, async: true
  use ExUnitProperties

  import Ash.SatSolver.Expression, only: [b: 1]

  alias Ash.SatSolver.Expression
  alias Ash.SatSolver.Expression.RewriteRule.AssociativityLaw

  doctest AssociativityLaw, import: true

  describe inspect(&AssociativityLaw.walk/1) do
    test "applies OR associativity: A OR (A OR B)" do
      assert Expression.postwalk(b(:a or (:a or :b)), &AssociativityLaw.walk/1) == b(:a or :b)
    end

    test "applies OR associativity: A OR B OR A" do
      assert Expression.postwalk(b(:a or :b or :a), &AssociativityLaw.walk/1) == b(:a or :b)
    end

    test "applies AND associativity: A AND (A AND B)" do
      assert Expression.postwalk(b(:a and (:a and :b)), &AssociativityLaw.walk/1) == b(:a and :b)
    end

    test "applies AND associativity: A AND B AND A" do
      assert Expression.postwalk(b(:a and :b and :a), &AssociativityLaw.walk/1) == b(:a and :b)
    end

    test "works with complex sub-expressions" do
      expr = b((:x and :y) or ((:x and :y) or :z))
      assert Expression.postwalk(expr, &AssociativityLaw.walk/1) == b((:x and :y) or :z)
    end

    test "works with negated expressions" do
      expr = b(not :a or (not :a or :b))
      assert Expression.postwalk(expr, &AssociativityLaw.walk/1) == b(not :a or :b)
    end

    test "handles complex nested associativity" do
      expr = b(:p and :q and (:p and :q and (:r and :s)))
      assert Expression.postwalk(expr, &AssociativityLaw.walk/1) == b(:p and :q and (:r and :s))
    end

    test "works with deeply nested expressions" do
      expr = b(((:a or :b) and :c) or (((:a or :b) and :c) or :d))
      assert Expression.postwalk(expr, &AssociativityLaw.walk/1) == b(((:a or :b) and :c) or :d)
    end

    test "handles multiple variables in associativity" do
      expr = b(:x or :y or :z or (:x or :y or :z or :w))
      assert Expression.postwalk(expr, &AssociativityLaw.walk/1) == b(:x or :y or :z or :w)
    end

    test "works with boolean constants" do
      expr = b(true or (true or false))
      assert Expression.postwalk(expr, &AssociativityLaw.walk/1) == b(true or false)
    end

    test "handles mixed boolean constants and variables" do
      expr = b((:a and false) or ((:a and false) or true))
      assert Expression.postwalk(expr, &AssociativityLaw.walk/1) == b((:a and false) or true)
    end

    test "works with complex nested boolean structures" do
      expr = b(not (:a or :b) and :c and (not (:a or :b) and :c and (:d or :e)))
      expected = b(not (:a or :b) and :c and (:d or :e))
      assert Expression.postwalk(expr, &AssociativityLaw.walk/1) == expected
    end

    test "leaves non-associativity patterns unchanged" do
      assert Expression.postwalk(b(:a and :b), &AssociativityLaw.walk/1) == b(:a and :b)
    end

    test "leaves different variables unchanged" do
      assert Expression.postwalk(b(:a or (:b or :c)), &AssociativityLaw.walk/1) ==
               b(:a or (:b or :c))
    end

    test "leaves single variables unchanged" do
      assert Expression.postwalk(:a, &AssociativityLaw.walk/1) == :a
    end

    test "leaves negations unchanged" do
      assert Expression.postwalk(b(not :a), &AssociativityLaw.walk/1) == b(not :a)
    end

    test "leaves boolean constants unchanged" do
      assert Expression.postwalk(true, &AssociativityLaw.walk/1)
      refute Expression.postwalk(false, &AssociativityLaw.walk/1)
    end

    test "handles partial associativity opportunities" do
      expr = b((:a or (:a or :b)) and (:c or :d))
      result = Expression.postwalk(expr, &AssociativityLaw.walk/1)
      assert result == b((:a or :b) and (:c or :d))
    end

    test "works with symmetric associativity patterns" do
      expr = b(:a and :b and ((:a and :b) or (:c and :d)))

      assert Expression.postwalk(expr, &AssociativityLaw.walk/1) ==
               b(:a and :b and ((:a and :b) or (:c and :d)))
    end

    test "handles multiple associativity opportunities" do
      expr = b((:a or (:a or :b)) and (:c and (:c and :d)))
      result = Expression.postwalk(expr, &AssociativityLaw.walk/1)
      assert result == b((:a or :b) and (:c and :d))
    end

    test "preserves expressions without associativity patterns" do
      expr = b((:a or :b) and (:c or :d))
      assert Expression.postwalk(expr, &AssociativityLaw.walk/1) == expr
    end

    test "works with triple associative expressions" do
      expr = b(:x or (:x or (:x or :y)))
      result = Expression.postwalk(expr, &AssociativityLaw.walk/1)
      assert result == b(:x or :y)
    end

    test "handles mixed AND/OR associativity" do
      expr = b((:a and (:a and :b)) or (:c or (:c or :d)))
      result = Expression.postwalk(expr, &AssociativityLaw.walk/1)
      assert result == b((:a and :b) or (:c or :d))
    end

    test "works with very complex identical sub-expressions" do
      complex_expr = b(not (:a or :b) and (:c or not :d))
      expr = b(complex_expr or (complex_expr or :simple))
      assert Expression.postwalk(expr, &AssociativityLaw.walk/1) == b(complex_expr or :simple)
    end

    test "preserves non-associative complex patterns" do
      expr = b((:a and :b) or (:c and (:a or :b)))
      assert Expression.postwalk(expr, &AssociativityLaw.walk/1) == expr
    end
  end

  describe "edge cases" do
    test "handles reverse associativity patterns" do
      exprs_and_expected = [
        {b(:x or (:x or :y)), b(:x or :y)},
        {b(:x or :y or :x), b(:x or :y)},
        {b(:x and (:x and :y)), b(:x and :y)},
        {b(:x and :y and :x), b(:x and :y)}
      ]

      for {expr, expected} <- exprs_and_expected do
        assert Expression.postwalk(expr, &AssociativityLaw.walk/1) == expected
      end
    end

    test "works with very complex identical sub-expressions" do
      complex_expr = b(not (:a and :b) or (:c and not :d))
      expr = b(complex_expr and (complex_expr and :simple))
      assert Expression.postwalk(expr, &AssociativityLaw.walk/1) == b(complex_expr and :simple)
    end

    test "preserves non-associative complex patterns" do
      expr = b(:a and :b and (:c or (:a and :b)))
      assert Expression.postwalk(expr, &AssociativityLaw.walk/1) == expr
    end

    test "handles deeply nested associative patterns" do
      expr = b(:a and (:b or :c) and (:a and (:b or :c) and (:d and :e)))
      expected = b(:a and (:b or :c) and (:d and :e))
      assert Expression.postwalk(expr, &AssociativityLaw.walk/1) == expected
    end

    test "works with multiple levels of associativity" do
      expr = b(:a or (:b or (:a or (:b or :c))))
      result = Expression.postwalk(expr, &AssociativityLaw.walk/1)
      assert result == b(:a or (:b or (:a or (:b or :c))))
    end
  end

  property "applying associativity law is idempotent" do
    check all(expr <- Expression.generate_expression(StreamData.atom(:alphanumeric))) do
      result1 = Expression.postwalk(expr, &AssociativityLaw.walk/1)
      result2 = Expression.postwalk(result1, &AssociativityLaw.walk/1)
      assert result1 == result2
    end
  end

  property "associativity law preserves logical equivalence" do
    check all(
            assignments <-
              StreamData.map_of(StreamData.atom(:alphanumeric), StreamData.boolean(),
                min_length: 1
              ),
            variable_names = Map.keys(assignments),
            expr <- Expression.generate_expression(StreamData.member_of(variable_names))
          ) do
      result = Expression.postwalk(expr, &AssociativityLaw.walk/1)
      eval_fn = &Map.fetch!(assignments, &1)

      assert Expression.run(expr, eval_fn) == Expression.run(result, eval_fn),
             """
             Associativity law changed the logical outcome!
             Original: #{inspect(expr, pretty: true)}
             Transformed: #{inspect(result, pretty: true)}
             Assignments: #{inspect(assignments, pretty: true)}
             """
    end
  end
end
