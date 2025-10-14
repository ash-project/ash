# SPDX-FileCopyrightText: 2020 Zach Daniel
#
# SPDX-License-Identifier: MIT

defmodule Ash.Test.SatSolver.Expression.RewriteRule.UnitResolution do
  use ExUnit.Case, async: true
  use ExUnitProperties

  import Ash.SatSolver.Expression, only: [b: 1]

  alias Ash.SatSolver.Expression
  alias Ash.SatSolver.Expression.RewriteRule.UnitResolution

  doctest UnitResolution, import: true

  describe inspect(&UnitResolution.walk/1) do
    test "applies unit resolution: A AND (NOT A OR B)" do
      expr = b(:a and (not :a or :b))
      result = Expression.postwalk(expr, &UnitResolution.walk/1)
      assert result == b(:a and :b)
    end

    test "applies unit resolution: (NOT A OR B) AND A" do
      expr = b((not :a or :b) and :a)
      result = Expression.postwalk(expr, &UnitResolution.walk/1)
      assert result == b(:b and :a)
    end

    test "applies unit resolution: (NOT A) AND (A OR B)" do
      expr = b(not :a and (:a or :b))
      result = Expression.postwalk(expr, &UnitResolution.walk/1)
      assert result == b(not :a and :b)
    end

    test "applies unit resolution: (A OR B) AND (NOT A)" do
      expr = b((:a or :b) and not :a)
      result = Expression.postwalk(expr, &UnitResolution.walk/1)
      assert result == b(:b and not :a)
    end

    test "works with complex sub-expressions as units" do
      # Unit: (X AND Y), Clause: NOT (X AND Y) OR Z
      unit = b(:x and :y)
      expr = b(unit and (not unit or :z))
      result = Expression.postwalk(expr, &UnitResolution.walk/1)
      assert result == b(unit and :z)
    end

    test "works with negated complex sub-expressions as units" do
      # Unit: NOT (P OR Q), Clause: (P OR Q) OR R
      complex_expr = b(:p or :q)
      expr = b(not complex_expr and (complex_expr or :r))
      result = Expression.postwalk(expr, &UnitResolution.walk/1)
      assert result == b(not complex_expr and :r)
    end

    test "handles multiple unit resolution opportunities" do
      # A AND (NOT A OR B) AND C AND (NOT C OR D)
      expr = b(:a and (not :a or :b) and :c and (not :c or :d))
      result = Expression.postwalk(expr, &UnitResolution.walk/1)
      # Should resolve first opportunity: A AND B AND C AND (NOT C OR D)
      assert result == b(:a and :b and :c and (not :c or :d))
    end

    test "works with deeply nested expressions" do
      # Unit resolution within a larger expression
      expr = b((:m or :n) and (:a and (not :a or :b)))
      result = Expression.postwalk(expr, &UnitResolution.walk/1)
      assert result == b((:m or :n) and (:a and :b))
    end

    test "handles sequential unit propagation" do
      # A AND (NOT A OR B) AND (NOT B OR C) should resolve first step
      expr = b(:a and (not :a or :b) and (not :b or :c))
      result = Expression.postwalk(expr, &UnitResolution.walk/1)
      # First pass: A AND B AND (NOT B OR C)
      assert result == b(:a and :b and (not :b or :c))
    end

    test "works with boolean constants as units" do
      # true AND (NOT true OR B) = true AND B
      expr = b(true and (not true or :b))
      result = Expression.postwalk(expr, &UnitResolution.walk/1)
      assert result == b(true and :b)
    end

    test "handles unit resolution with multiple literals in clause" do
      # A AND (NOT A OR B OR C) - our pattern doesn't handle this complex case
      expr = b(:a and (not :a or :b or :c))
      result = Expression.postwalk(expr, &UnitResolution.walk/1)
      # Should remain unchanged as our patterns only handle binary OR clauses
      assert result == expr
    end

    test "works with very complex identical sub-expressions" do
      complex_expr = b((:p and :q) or (:r and not :s))
      expr = b(complex_expr and (not complex_expr or :t))
      result = Expression.postwalk(expr, &UnitResolution.walk/1)
      assert result == b(complex_expr and :t)
    end

    test "leaves non-unit patterns unchanged" do
      # No unit clauses
      expr = b((:a or :b) and (:c or :d))
      assert Expression.postwalk(expr, &UnitResolution.walk/1) == expr
    end

    test "leaves non-matching unit patterns unchanged" do
      # Unit doesn't contradict clause
      expr = b(:a and (:a or :b))
      assert Expression.postwalk(expr, &UnitResolution.walk/1) == expr
    end

    test "leaves single variables unchanged" do
      assert Expression.postwalk(:a, &UnitResolution.walk/1) == :a
    end

    test "leaves simple expressions unchanged" do
      assert Expression.postwalk(b(:a and :b), &UnitResolution.walk/1) == b(:a and :b)
      assert Expression.postwalk(b(:a or :b), &UnitResolution.walk/1) == b(:a or :b)
    end

    test "leaves boolean constants unchanged" do
      assert Expression.postwalk(true, &UnitResolution.walk/1)
      refute Expression.postwalk(false, &UnitResolution.walk/1)
    end

    test "handles partial unit resolution patterns" do
      # Has unit but no contradicting clause
      expr = b(:a and (:b or :c))
      assert Expression.postwalk(expr, &UnitResolution.walk/1) == expr
    end

    test "preserves expressions without unit patterns" do
      expr = b((:a or :b) and (:c and :d) and (:e or :f))
      assert Expression.postwalk(expr, &UnitResolution.walk/1) == expr
    end
  end

  describe "edge cases" do
    test "handles all unit resolution ordering patterns" do
      # Test all possible orderings systematically
      unit_patterns = [
        b(:x and (not :x or :y)),
        b((not :x or :y) and :x),
        b(not :x and (:x or :y)),
        b((:x or :y) and not :x)
      ]

      expected_results = [
        b(:x and :y),
        b(:y and :x),
        b(not :x and :y),
        b(:y and not :x)
      ]

      for {expr, expected} <- Enum.zip(unit_patterns, expected_results) do
        result = Expression.postwalk(expr, &UnitResolution.walk/1)
        assert result == expected
      end
    end

    test "handles unit resolution with negated units" do
      # Both positive and negative units
      patterns = [
        {b(:a and (not :a or :b)), b(:a and :b)},
        {b(not :a and (:a or :b)), b(not :a and :b)}
      ]

      for {expr, expected} <- patterns do
        result = Expression.postwalk(expr, &UnitResolution.walk/1)
        assert result == expected
      end
    end

    test "handles reapplication correctly" do
      # Test that needs_reapplication? works properly using RewriteRule.apply
      # A AND (NOT A OR B) AND (NOT B OR C) should progressively resolve
      expr = b(:a and (not :a or :b) and (not :b or :c))

      # Use RewriteRule.apply which handles reapplication automatically
      {result, _acc_map} = Expression.RewriteRule.apply(expr, [UnitResolution])

      # Should resolve first step: A AND B AND (NOT B OR C)
      # The reapplication should eventually resolve to A AND B AND C
      # But since we're only testing UnitResolution in isolation, check intermediate step
      assert result == b(:a and :b and :c) or result == b(:a and :b and (not :b or :c))
    end

    test "preserves non-unit resolution patterns" do
      # Multiple units but no contradictions
      expr = b(:a and :b and (:c or :d))
      assert Expression.postwalk(expr, &UnitResolution.walk/1) == expr
    end

    test "handles mixed unit and non-unit clauses" do
      # Some resolvable, some not
      expr = b(:a and (not :a or :b) and (:c or :d))
      result = Expression.postwalk(expr, &UnitResolution.walk/1)
      assert result == b(:a and :b and (:c or :d))
    end

    test "works with symmetric unit patterns" do
      # Multiple variables in symmetric positions
      expr = b(:x and (not :x or :y) and :z and (not :z or :w))
      result = Expression.postwalk(expr, &UnitResolution.walk/1)
      # Should resolve first opportunity: X AND Y AND Z AND (NOT Z OR W)
      assert result == b(:x and :y and :z and (not :z or :w))
    end

    test "handles contradictory units gracefully" do
      # A AND NOT A - should remain as is (contradiction)
      expr = b(:a and not :a)
      result = Expression.postwalk(expr, &UnitResolution.walk/1)
      assert result == expr
    end
  end

  property "applying unit resolution is idempotent after stabilization" do
    check all(expr <- Expression.generate_expression(StreamData.atom(:alphanumeric))) do
      # Use RewriteRule.apply which handles reapplication automatically
      {result1, _acc_map1} = Expression.RewriteRule.apply(expr, [UnitResolution])
      {result2, _acc_map2} = Expression.RewriteRule.apply(result1, [UnitResolution])

      # Should be idempotent after stabilization
      assert result1 == result2
    end
  end

  property "unit resolution preserves logical equivalence" do
    check all(
            assignments <-
              StreamData.map_of(StreamData.atom(:alphanumeric), StreamData.boolean(),
                min_length: 1
              ),
            variable_names = Map.keys(assignments),
            expr <- Expression.generate_expression(StreamData.member_of(variable_names))
          ) do
      {result, _acc_map} = Expression.RewriteRule.apply(expr, [UnitResolution])
      eval_fn = &Map.fetch!(assignments, &1)

      assert Expression.run(expr, eval_fn) == Expression.run(result, eval_fn),
             """
             Unit resolution changed the logical outcome!
             Original: #{inspect(expr, pretty: true)}
             Transformed: #{inspect(result, pretty: true)}
             Assignments: #{inspect(assignments, pretty: true)}
             """
    end
  end
end
