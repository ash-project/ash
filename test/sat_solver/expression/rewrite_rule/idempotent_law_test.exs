# SPDX-FileCopyrightText: 2020 Zach Daniel
#
# SPDX-License-Identifier: MIT

# credo:disable-for-this-file Credo.Check.Warning.BoolOperationOnSameValues
defmodule Ash.Test.SatSolver.Expression.RewriteRule.IdempotentLaw do
  use ExUnit.Case, async: true
  use ExUnitProperties

  import Ash.SatSolver.Expression, only: [b: 1]

  alias Ash.SatSolver.Expression
  alias Ash.SatSolver.Expression.RewriteRule.IdempotentLaw

  doctest IdempotentLaw, import: true

  describe inspect(&IdempotentLaw.walk/1) do
    test "applies AND idempotent law" do
      assert Expression.postwalk(b(:a and :a), &IdempotentLaw.walk/1) == :a
    end

    test "applies OR idempotent law" do
      assert Expression.postwalk(b(:a or :a), &IdempotentLaw.walk/1) == :a
    end

    test "works with complex sub-expressions" do
      assert Expression.postwalk(b(:a and :b and (:a and :b)), &IdempotentLaw.walk/1) ==
               b(:a and :b)
    end

    test "works with complex OR expressions" do
      assert Expression.postwalk(b(:x or :y or (:x or :y)), &IdempotentLaw.walk/1) == b(:x or :y)
    end

    test "handles nested expressions" do
      assert Expression.postwalk(b(not :a and not :a), &IdempotentLaw.walk/1) == b(not :a)
    end

    test "works with negated complex expressions" do
      assert Expression.postwalk(b(not (:a and :b) or not (:a and :b)), &IdempotentLaw.walk/1) ==
               b(not (:a and :b))
    end

    test "handles deeply nested identical expressions" do
      expr = b(:a and (:b or :c) and (:a and (:b or :c)))
      assert Expression.postwalk(expr, &IdempotentLaw.walk/1) == b(:a and (:b or :c))
    end

    test "works with boolean constants" do
      assert Expression.postwalk(b(true and true), &IdempotentLaw.walk/1)
      refute Expression.postwalk(b(false and false), &IdempotentLaw.walk/1)
      assert Expression.postwalk(b(true or true), &IdempotentLaw.walk/1)
      refute Expression.postwalk(b(false or false), &IdempotentLaw.walk/1)
    end

    test "handles multiple variables in identical expressions" do
      assert Expression.postwalk(
               b((:a and :b and :c) or (:a and :b and :c)),
               &IdempotentLaw.walk/1
             ) ==
               b(:a and :b and :c)
    end

    test "works with complex nested structures" do
      expr = b((:a or :b) and (:c or :d) and ((:a or :b) and (:c or :d)))
      result = Expression.postwalk(expr, &IdempotentLaw.walk/1)
      assert result == b((:a or :b) and (:c or :d))
    end

    test "handles mixed AND and OR in identical expressions" do
      expr = b((:a and (:b or :c)) or (:a and (:b or :c)))
      assert Expression.postwalk(expr, &IdempotentLaw.walk/1) == b(:a and (:b or :c))
    end

    test "leaves non-identical patterns unchanged" do
      assert Expression.postwalk(b(:a and :b), &IdempotentLaw.walk/1) == b(:a and :b)
    end

    test "leaves different variables unchanged" do
      assert Expression.postwalk(b(:a or :b), &IdempotentLaw.walk/1) == b(:a or :b)
    end

    test "leaves single variables unchanged" do
      assert Expression.postwalk(:a, &IdempotentLaw.walk/1) == :a
    end

    test "leaves negations unchanged" do
      assert Expression.postwalk(b(not :a), &IdempotentLaw.walk/1) == b(not :a)
    end

    test "leaves boolean constants unchanged" do
      assert Expression.postwalk(true, &IdempotentLaw.walk/1)
      refute Expression.postwalk(false, &IdempotentLaw.walk/1)
    end

    test "handles partial identical structures" do
      # Only part of the expression is identical - should not apply
      expr = b((:a and :a) or (:b and :c))
      result = Expression.postwalk(expr, &IdempotentLaw.walk/1)
      assert result == b(:a or (:b and :c))
    end

    test "works with very complex identical expressions" do
      expr = b((not (:a and :b) or (:c and not :d)) and (not (:a and :b) or (:c and not :d)))
      expected = b(not (:a and :b) or (:c and not :d))
      assert Expression.postwalk(expr, &IdempotentLaw.walk/1) == expected
    end

    test "handles multiple idempotent opportunities" do
      expr = b((:a and :a) or (:b or :b))
      result = Expression.postwalk(expr, &IdempotentLaw.walk/1)
      assert result == b(:a or :b)
    end

    test "preserves expression structure for non-idempotent cases" do
      expr = b((:a and :b) or (:c and :d))
      assert Expression.postwalk(expr, &IdempotentLaw.walk/1) == expr
    end
  end

  describe "edge cases" do
    test "handles triple identical expressions" do
      # The rule applies recursively, so this fully reduces
      expr = b(:a and :a and :a)
      result = Expression.postwalk(expr, &IdempotentLaw.walk/1)
      # This reduces fully to :a through recursive application
      assert result == :a
    end

    test "works with identical expressions at different nesting levels" do
      expr = b((:a and :b and (:a and :b)) or :c)
      result = Expression.postwalk(expr, &IdempotentLaw.walk/1)
      assert result == b((:a and :b) or :c)
    end

    test "preserves non-idempotent complex patterns" do
      expr = b(:a and :b and (:a or :b))
      assert Expression.postwalk(expr, &IdempotentLaw.walk/1) == expr
    end
  end

  property "applying idempotent law is idempotent" do
    check all(expr <- Expression.generate_expression(StreamData.atom(:alphanumeric))) do
      result1 = Expression.postwalk(expr, &IdempotentLaw.walk/1)
      result2 = Expression.postwalk(result1, &IdempotentLaw.walk/1)
      assert result1 == result2
    end
  end

  property "idempotent laws preserve logical equivalence" do
    check all(
            assignments <-
              StreamData.map_of(StreamData.atom(:alphanumeric), StreamData.boolean(),
                min_length: 1
              ),
            variable_names = Map.keys(assignments),
            expr <- Expression.generate_expression(StreamData.member_of(variable_names))
          ) do
      result = Expression.postwalk(expr, &IdempotentLaw.walk/1)
      eval_fn = &Map.fetch!(assignments, &1)

      assert Expression.run(expr, eval_fn) == Expression.run(result, eval_fn),
             """
             Idempotent law changed the logical outcome!
             Original: #{inspect(expr, pretty: true)}
             Transformed: #{inspect(result, pretty: true)}
             Assignments: #{inspect(assignments, pretty: true)}
             """
    end
  end
end
