# SPDX-FileCopyrightText: ash contributors <https://github.com/ash-project/ash/graphs.contributors>
#
#
# SPDX-License-Identifier: MIT

defmodule Ash.Test.SatSolver.Expression.RewriteRule.ConsensusTheorem do
  use ExUnit.Case, async: true
  use ExUnitProperties

  import Ash.SatSolver.Expression, only: [b: 1]

  alias Ash.SatSolver.Expression
  alias Ash.SatSolver.Expression.RewriteRule.ConsensusTheorem

  doctest ConsensusTheorem, import: true

  describe inspect(&ConsensusTheorem.walk/1) do
    test "applies consensus theorem: (A OR B) AND (NOT A OR C) AND (B OR C)" do
      expr = b((:a or :b) and (not :a or :c) and (:b or :c))
      result = Expression.postwalk(expr, &ConsensusTheorem.walk/1)
      assert result == b((:a or :b) and (not :a or :c))
    end

    test "applies consensus theorem: (NOT A OR C) AND (A OR B) AND (B OR C)" do
      expr = b((not :a or :c) and (:a or :b) and (:b or :c))
      result = Expression.postwalk(expr, &ConsensusTheorem.walk/1)
      assert result == b((not :a or :c) and (:a or :b))
    end

    test "applies consensus theorem: (B OR C) AND (A OR B) AND (NOT A OR C)" do
      expr = b((:b or :c) and (:a or :b) and (not :a or :c))
      result = Expression.postwalk(expr, &ConsensusTheorem.walk/1)
      assert result == b((:a or :b) and (not :a or :c))
    end

    test "applies consensus theorem: (A AND B) OR (NOT A AND C) OR (B AND C)" do
      expr = b((:a and :b) or (not :a and :c) or (:b and :c))
      result = Expression.postwalk(expr, &ConsensusTheorem.walk/1)
      assert result == b((:a and :b) or (not :a and :c))
    end

    test "applies consensus theorem: (NOT A AND C) OR (A AND B) OR (B AND C)" do
      expr = b((not :a and :c) or (:a and :b) or (:b and :c))
      result = Expression.postwalk(expr, &ConsensusTheorem.walk/1)
      assert result == b((not :a and :c) or (:a and :b))
    end

    test "applies consensus theorem: (B AND C) OR (A AND B) OR (NOT A AND C)" do
      expr = b((:b and :c) or (:a and :b) or (not :a and :c))
      result = Expression.postwalk(expr, &ConsensusTheorem.walk/1)
      assert result == b((:a and :b) or (not :a and :c))
    end

    test "works with complex sub-expressions" do
      # Using (X AND Y) as A, Z as B, W as C
      expr = b(((:x and :y) or :z) and (not (:x and :y) or :w) and (:z or :w))
      result = Expression.postwalk(expr, &ConsensusTheorem.walk/1)
      assert result == b(((:x and :y) or :z) and (not (:x and :y) or :w))
    end

    test "works with negated complex sub-expressions" do
      # Using NOT (P OR Q) as A, R as B, S as C
      complex_a = b(not (:p or :q))
      expr = b((complex_a or :r) and (not complex_a or :s) and (:r or :s))
      result = Expression.postwalk(expr, &ConsensusTheorem.walk/1)
      assert result == b((complex_a or :r) and (not complex_a or :s))
    end

    test "handles multiple consensus opportunities" do
      # Two separate consensus patterns in one expression
      expr =
        b(
          (:a or :b) and (not :a or :c) and (:b or :c) and
            ((:x or :y) and (not :x or :z) and (:y or :z))
        )

      result = Expression.postwalk(expr, &ConsensusTheorem.walk/1)
      assert result == b((:a or :b) and (not :a or :c) and ((:x or :y) and (not :x or :z)))
    end

    test "works with deeply nested expressions" do
      # Consensus within a larger expression
      expr = b((:m and :n) or ((:a or :b) and (not :a or :c) and (:b or :c)))
      result = Expression.postwalk(expr, &ConsensusTheorem.walk/1)
      assert result == b((:m and :n) or ((:a or :b) and (not :a or :c)))
    end

    test "leaves non-consensus patterns unchanged" do
      # Missing the third clause
      expr = b((:a or :b) and (not :a or :c))
      assert Expression.postwalk(expr, &ConsensusTheorem.walk/1) == expr
    end

    test "leaves different variable patterns unchanged" do
      # Variables don't match consensus pattern
      expr = b((:a or :b) and (not :c or :d) and (:e or :f))
      assert Expression.postwalk(expr, &ConsensusTheorem.walk/1) == expr
    end

    test "leaves single variables unchanged" do
      assert Expression.postwalk(:a, &ConsensusTheorem.walk/1) == :a
    end

    test "leaves simple expressions unchanged" do
      assert Expression.postwalk(b(:a and :b), &ConsensusTheorem.walk/1) == b(:a and :b)
      assert Expression.postwalk(b(:a or :b), &ConsensusTheorem.walk/1) == b(:a or :b)
    end

    test "leaves boolean constants unchanged" do
      assert Expression.postwalk(true, &ConsensusTheorem.walk/1)
      refute Expression.postwalk(false, &ConsensusTheorem.walk/1)
    end

    test "handles partial consensus patterns" do
      # Has two consensus clauses but third doesn't match
      expr = b((:a or :b) and (not :a or :c) and (:x or :y))
      assert Expression.postwalk(expr, &ConsensusTheorem.walk/1) == expr
    end

    test "works with very complex identical sub-expressions" do
      complex_expr = b((:p and :q) or (:r and not :s))
      expr = b((complex_expr or :t) and (not complex_expr or :u) and (:t or :u))
      result = Expression.postwalk(expr, &ConsensusTheorem.walk/1)
      assert result == b((complex_expr or :t) and (not complex_expr or :u))
    end

    test "preserves non-consensus complex patterns" do
      expr = b((:a and :b) or (:c and :d) or (:e and :f))
      assert Expression.postwalk(expr, &ConsensusTheorem.walk/1) == expr
    end
  end

  describe "edge cases" do
    test "handles all consensus ordering patterns" do
      # Test all possible orderings systematically
      base_clauses = [
        {:a_or_b, b((:a or :b) and (not :a or :c) and (:b or :c))},
        {:not_a_or_c, b((not :a or :c) and (:a or :b) and (:b or :c))},
        {:b_or_c, b((:b or :c) and (:a or :b) and (not :a or :c))}
      ]

      for {_label, expr} <- base_clauses do
        result = Expression.postwalk(expr, &ConsensusTheorem.walk/1)
        # CommutativityLaw may reorder clauses, so check for logical equivalence
        expected = b((:a or :b) and (not :a or :c))
        assert result == expected or result == b((not :a or :c) and (:a or :b))
      end
    end

    test "handles consensus with boolean constants" do
      # A = true, B = :b, C = :c
      expr = b((true or :b) and (not true or :c) and (:b or :c))
      result = Expression.postwalk(expr, &ConsensusTheorem.walk/1)
      assert result == b((true or :b) and (not true or :c))
    end

    test "preserves expressions without consensus patterns" do
      expr = b((:a or :b) and (:c or :d) and (:e or :f))
      assert Expression.postwalk(expr, &ConsensusTheorem.walk/1) == expr
    end

    test "handles mixed AND and OR operations" do
      # Should not match consensus (mixing AND/OR incorrectly)
      expr = b(:a or :b or (not :a and :c) or (:b and :c))
      assert Expression.postwalk(expr, &ConsensusTheorem.walk/1) == expr
    end

    test "works with symmetric consensus patterns" do
      # Multiple variables in symmetric positions
      expr = b((:x or :y) and (not :x or :z) and (:y or :z))
      result = Expression.postwalk(expr, &ConsensusTheorem.walk/1)
      assert result == b((:x or :y) and (not :x or :z))
    end
  end

  property "applying consensus theorem is idempotent" do
    check all(expr <- Expression.generate_expression(StreamData.atom(:alphanumeric))) do
      result1 = Expression.postwalk(expr, &ConsensusTheorem.walk/1)
      result2 = Expression.postwalk(result1, &ConsensusTheorem.walk/1)
      assert result1 == result2
    end
  end

  property "consensus theorem preserves logical equivalence" do
    check all(
            assignments <-
              StreamData.map_of(StreamData.atom(:alphanumeric), StreamData.boolean(),
                min_length: 1
              ),
            variable_names = Map.keys(assignments),
            expr <- Expression.generate_expression(StreamData.member_of(variable_names))
          ) do
      result = Expression.postwalk(expr, &ConsensusTheorem.walk/1)
      eval_fn = &Map.fetch!(assignments, &1)

      assert Expression.run(expr, eval_fn) == Expression.run(result, eval_fn),
             """
             Consensus theorem changed the logical outcome!
             Original: #{inspect(expr, pretty: true)}
             Transformed: #{inspect(result, pretty: true)}
             Assignments: #{inspect(assignments, pretty: true)}
             """
    end
  end
end
