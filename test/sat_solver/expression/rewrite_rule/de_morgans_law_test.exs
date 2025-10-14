# SPDX-FileCopyrightText: ash contributors <https://github.com/ash-project/ash/graphs.contributors>
#
#
# SPDX-License-Identifier: MIT

defmodule Ash.Test.SatSolver.Expression.RewriteRule.DeMorgansLaw do
  use ExUnit.Case, async: true
  use ExUnitProperties

  import Ash.SatSolver.Expression, only: [b: 1]

  alias Ash.SatSolver.Expression
  alias Ash.SatSolver.Expression.RewriteRule
  alias Ash.SatSolver.Expression.RewriteRule.DeMorgansLaw

  doctest DeMorgansLaw, import: true

  describe inspect(&DeMorgansLaw.walk/1) do
    test "transforms NOT (A AND B)" do
      expr = b(not (:a and :b))
      {result, _acc_map} = RewriteRule.apply(expr, [DeMorgansLaw])
      assert result == b(not :a or not :b)
    end

    test "transforms NOT (A OR B)" do
      expr = b(not (:a or :b))
      {result, _acc_map} = RewriteRule.apply(expr, [DeMorgansLaw])
      assert result == b(not :a and not :b)
    end

    test "handles nested applications requiring reapplication" do
      expr = b(not ((:a and :b) or (:c and :d)))
      {result, _acc_map} = RewriteRule.apply(expr, [DeMorgansLaw])
      # Should apply De Morgan's to outer OR, then to inner ANDs
      assert result == b((not :a or not :b) and (not :c or not :d))
    end

    test "leaves non-matching patterns unchanged" do
      expr = b(not :a)
      {result, _acc_map} = RewriteRule.apply(expr, [DeMorgansLaw])
      assert result == b(not :a)
    end

    test "works with complex nested expressions" do
      expr = b(not ((not :a and :b) or (not :c and :d)))
      {result, _acc_map} = RewriteRule.apply(expr, [DeMorgansLaw])
      assert result == b((not not :a or not :b) and (not not :c or not :d))
    end

    test "handles multiple separate applications" do
      expr = b(not (:a and :b) or not (:c or :d))
      {result, _acc_map} = RewriteRule.apply(expr, [DeMorgansLaw])
      assert result == b(not :a or not :b or (not :c and not :d))
    end

    test "applies to deeply nested conjunctions" do
      expr = b(not (:a and :b and (:c and :d)))
      {result, _acc_map} = RewriteRule.apply(expr, [DeMorgansLaw])
      # Should distribute through all levels
      assert result == b(not :a or not :b or (not :c or not :d))
    end

    test "applies to deeply nested disjunctions" do
      expr = b(not (:a or :b or (:c or :d)))
      {result, _acc_map} = RewriteRule.apply(expr, [DeMorgansLaw])
      # Should distribute through all levels
      assert result == b(not :a and not :b and (not :c and not :d))
    end

    test "works with boolean constants" do
      expr = b(not (true and false))
      {result, _acc_map} = RewriteRule.apply(expr, [DeMorgansLaw])
      assert result == b(not true or not false)
    end

    test "handles mixed AND and OR patterns" do
      expr = b(not ((:a and :b) or (:c and :d) or (:e and :f)))
      {result, _acc_map} = RewriteRule.apply(expr, [DeMorgansLaw])
      # Should apply De Morgan's recursively to break down the structure
      expected = b((not :a or not :b) and (not :c or not :d) and (not :e or not :f))
      assert result == expected
    end

    test "preserves expressions that don't match patterns" do
      expr = b(:a and (:b or :c))
      {result, _acc_map} = RewriteRule.apply(expr, [DeMorgansLaw])
      assert result == b(:a and (:b or :c))
    end

    test "handles single variable negations" do
      expr = b(not :x and not :y)
      {result, _acc_map} = RewriteRule.apply(expr, [DeMorgansLaw])
      assert result == b(not :x and not :y)
    end
  end

  describe "reapplication behavior" do
    test "continues applying until no more transformations possible" do
      expr = b(not (not (:a and :b) and not (:c or :d)))
      {result, _acc_map} = RewriteRule.apply(expr, [DeMorgansLaw])
      # Should keep applying De Morgan's laws until exhausted
      expected = b((not not :a and not not :b) or (not not :c or not not :d))
      assert result == expected
    end

    test "handles complex nested patterns requiring multiple passes" do
      expr = b(not (not (:a and :b) and not (:c or :d)))
      {result, _acc_map} = RewriteRule.apply(expr, [DeMorgansLaw])
      # Should apply De Morgan's at multiple levels
      expected = b((not not :a and not not :b) or (not not :c or not not :d))
      assert result == expected
    end
  end

  property "De Morgan's laws preserve logical equivalence" do
    check all(
            assignments <-
              StreamData.map_of(StreamData.atom(:alphanumeric), StreamData.boolean(),
                min_length: 1
              ),
            variable_names = Map.keys(assignments),
            expr <- Expression.generate_expression(StreamData.member_of(variable_names))
          ) do
      {result, _acc_map} = RewriteRule.apply(expr, [DeMorgansLaw])
      eval_fn = &Map.fetch!(assignments, &1)

      assert Expression.run(expr, eval_fn) == Expression.run(result, eval_fn),
             """
             De Morgan's law changed the logical outcome!
             Original: #{inspect(expr, pretty: true)}
             Transformed: #{inspect(result, pretty: true)}
             Assignments: #{inspect(assignments, pretty: true)}
             """
    end
  end

  property "applying De Morgan's laws is idempotent after first application" do
    check all(expr <- Expression.generate_expression(StreamData.atom(:alphanumeric))) do
      {result1, _acc_map1} = RewriteRule.apply(expr, [DeMorgansLaw])
      {result2, _acc_map2} = RewriteRule.apply(result1, [DeMorgansLaw])
      assert result1 == result2
    end
  end
end
