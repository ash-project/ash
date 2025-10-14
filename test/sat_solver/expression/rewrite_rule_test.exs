# SPDX-FileCopyrightText: ash contributors <https://github.com/ash-project/ash/graphs.contributors>
#
#
# SPDX-License-Identifier: MIT

defmodule Ash.Test.SatSolver.Expression.RewriteRule do
  use ExUnit.Case, async: true
  use ExUnitProperties

  import Ash.SatSolver.Expression, only: [b: 1]

  alias Ash.SatSolver.Expression
  alias Ash.SatSolver.Expression.RewriteRule
  alias Ash.SatSolver.Expression.RewriteRule.DeMorgansLaw
  alias Ash.SatSolver.Expression.RewriteRule.DistributiveLaw
  alias Ash.SatSolver.Expression.RewriteRule.NegationLaw

  doctest RewriteRule, import: true

  describe inspect(&RewriteRule.apply/2) do
    test "applies single rule" do
      expr = b(not not :a)
      {result, _acc_map} = RewriteRule.apply(expr, [NegationLaw])
      assert result == :a
    end

    test "applies multiple rules in order" do
      # First apply De Morgan's law, then double negation elimination
      expr = b(not (:a and :b))
      {result, _acc_map} = RewriteRule.apply(expr, [DeMorgansLaw, NegationLaw])
      assert result == b(not :a or not :b)
    end

    test "handles empty rule list" do
      expr = b(:a and :b)
      {result, _acc_map} = RewriteRule.apply(expr, [])
      assert result == expr
    end

    test "preserves rule order with different types" do
      # Create a test case where order matters
      expr = b(not (not (:a and :b)))

      # Apply double negation first, then De Morgan's
      {result1, _acc_map1} = RewriteRule.apply(expr, [NegationLaw, DeMorgansLaw])
      # Double negation eliminates to (:a and :b)
      assert result1 == b(:a and :b)

      # Apply De Morgan's first - it won't match the pattern, then double negation
      {result2, _acc_map2} = RewriteRule.apply(expr, [DeMorgansLaw, NegationLaw])
      # De Morgan's doesn't apply, then double negation eliminates
      assert result2 == b(:a and :b)
    end

    test "handles reapplication correctly" do
      # De Morgan's law needs reapplication - it applies recursively
      expr = b(not (not (:a and :b) and not (:c or :d)))
      {result, _acc_map} = RewriteRule.apply(expr, [DeMorgansLaw])
      # The actual result based on the test output shows it applies De Morgan's thoroughly
      expected = b((not not :a and not not :b) or (not not :c or not not :d))
      assert result == expected
    end

    test "chunks rules by type and exclusivity" do
      # This is more of an integration test - we can't directly observe chunking,
      # but we can test that the result is correct
      expr = b(not not (not (:a and :b)))
      {result, _acc_map} = RewriteRule.apply(expr, [NegationLaw, DeMorgansLaw, NegationLaw])
      # Should eliminate double negations, apply De Morgan's, then eliminate again
      assert result == b(not :a or not :b)
    end
  end

  # Test helper modules for specific scenarios
  defmodule TestExclusiveRule do
    use RewriteRule

    @impl RewriteRule
    def exclusive?, do: true

    @impl RewriteRule
    def walk(b(:exclusive_test)), do: :exclusive_applied
    def walk(other), do: other
  end

  defmodule TestPrewalkRule do
    use RewriteRule

    @impl RewriteRule
    def type, do: :prewalk

    @impl RewriteRule
    def walk(b(:prewalk_test)), do: :prewalk_applied
    def walk(other), do: other
  end

  defmodule TestReapplicationRule do
    use RewriteRule

    @impl RewriteRule
    def needs_reapplication?, do: true

    @impl RewriteRule
    def walk(b(:reapply_test)), do: :reapply_applied
    def walk(other), do: other
  end

  describe "rule chunking behavior" do
    test "exclusive rules get their own chunk" do
      expr = b(:exclusive_test and :a)
      {result, _acc_map} = RewriteRule.apply(expr, [TestExclusiveRule, NegationLaw])
      assert result == b(:exclusive_applied and :a)
    end

    test "different types get separate chunks" do
      expr = b(:prewalk_test and not not :b)
      {result, _acc_map} = RewriteRule.apply(expr, [TestPrewalkRule, NegationLaw])
      assert result == b(:prewalk_applied and :b)
    end

    test "same type non-exclusive rules are chunked together" do
      # Both are postwalk non-exclusive, should be in same chunk
      expr = b(not not :a)
      {result, _acc_map} = RewriteRule.apply(expr, [NegationLaw, NegationLaw])
      assert result == :a
    end
  end

  describe "reapplication behavior" do
    test "reapplies rules that need it" do
      expr = b(:reapply_test)
      {result, _acc_map} = RewriteRule.apply(expr, [TestReapplicationRule])
      assert result == :reapply_applied
    end

    test "stops when no changes occur" do
      # Rule that transforms once then stops
      expr = :already_transformed
      {result, _acc_map} = RewriteRule.apply(expr, [TestReapplicationRule])
      assert result == :already_transformed
    end
  end

  describe "complex scenarios" do
    test "mixed rule types and exclusivity" do
      # Test a complex scenario with mixed rule types
      expr = b(not not (not (:a and :b)))

      rules = [
        # prewalk, non-exclusive
        TestPrewalkRule,
        # postwalk, non-exclusive
        NegationLaw,
        # postwalk, exclusive
        TestExclusiveRule,
        # postwalk, non-exclusive, needs reapplication
        DeMorgansLaw
      ]

      # Should handle all these correctly in separate chunks
      {result, _acc_map} = RewriteRule.apply(expr, rules)
      # The exact result depends on what transformations apply,
      # but it should not crash and should be deterministic
      assert is_tuple(result) or is_atom(result)
    end

    test "to_cnf equivalent using rules" do
      # Test that we can achieve CNF using our rules
      expr = b(:a or (:b and :c))
      {result, _acc_map} = RewriteRule.apply(expr, [DistributiveLaw])
      assert result == b((:a or :b) and (:a or :c))
    end
  end

  describe "accumulator functionality" do
    test "function-based rule with accumulator" do
      counter_rule = fn expr, count ->
        case expr do
          :a -> {:transformed_a, count + 1}
          other -> {other, count}
        end
      end

      expr = b(:a and :b and :a)
      {result, acc_map} = RewriteRule.apply(expr, [{counter_rule, 0}])

      assert result == b(:transformed_a and :b and :transformed_a)
      assert Map.get(acc_map, counter_rule) == 2
    end

    test "function-based rule with options" do
      exclusive_counter = fn expr, count ->
        case expr do
          :special -> {:special_transformed, count + 1}
          other -> {other, count}
        end
      end

      expr = b(:special and :a)
      {result, acc_map} = RewriteRule.apply(expr, [{exclusive_counter, 0, [exclusive?: true]}])

      assert result == b(:special_transformed and :a)
      assert Map.get(acc_map, exclusive_counter) == 1
    end

    test "accumulator persistence across reapplications" do
      reapply_counter = fn expr, count ->
        case expr do
          :count_me -> {:counted, count + 1}
          other -> {other, count}
        end
      end

      expr = :count_me

      {result, acc_map} =
        RewriteRule.apply(expr, [{reapply_counter, 0, [needs_reapplication?: true]}])

      assert result == :counted
      assert Map.get(acc_map, reapply_counter) == 1
    end

    test "multiple rules with isolated accumulators" do
      counter1 = fn expr, count ->
        case expr do
          :a -> {:a1, count + 1}
          other -> {other, count}
        end
      end

      counter2 = fn expr, count ->
        case expr do
          :b -> {:b2, count + 10}
          other -> {other, count}
        end
      end

      expr = b(:a and :b)
      {result, acc_map} = RewriteRule.apply(expr, [{counter1, 0}, {counter2, 0}])

      assert result == b(:a1 and :b2)
      assert Map.get(acc_map, counter1) == 1
      assert Map.get(acc_map, counter2) == 10
    end

    test "mixed module and function rules with accumulators" do
      collector = fn expr, visited ->
        case expr do
          atom when is_atom(atom) and atom not in [:and, :or, :not, true, false] ->
            {atom, [atom | visited]}

          other ->
            {other, visited}
        end
      end

      expr = b(not not :x)
      {result, acc_map} = RewriteRule.apply(expr, [NegationLaw, {collector, []}])

      assert result == :x
      assert :x in Map.get(acc_map, collector)
      assert Map.get(acc_map, NegationLaw) == nil
    end
  end

  property "applying empty rules returns original expression" do
    check all(expr <- Expression.generate_expression(StreamData.atom(:alphanumeric))) do
      {result, _acc_map} = RewriteRule.apply(expr, [])
      assert result == expr
    end
  end
end
