# SPDX-FileCopyrightText: ash contributors <https://github.com/ash-project/ash/graphs.contributors>
#
#
# SPDX-License-Identifier: MIT

defmodule Ash.Test.SatSolver.Expression.RewriteRule.TautologyLaw do
  use ExUnit.Case, async: true
  use ExUnitProperties

  import Ash.SatSolver.Expression, only: [b: 1]

  alias Ash.SatSolver.Expression
  alias Ash.SatSolver.Expression.RewriteRule.TautologyLaw

  doctest TautologyLaw, import: true

  describe inspect(&TautologyLaw.walk/1) do
    test "applies tautology: A OR (NOT A OR B)" do
      expr = b(:a or (not :a or :b))
      result = Expression.postwalk(expr, &TautologyLaw.walk/1)
      assert result
    end

    test "applies tautology: A OR (B OR NOT A)" do
      expr = b(:a or (:b or not :a))
      result = Expression.postwalk(expr, &TautologyLaw.walk/1)
      assert result
    end

    test "applies tautology: (NOT A OR B) OR A" do
      expr = b(not :a or :b or :a)
      result = Expression.postwalk(expr, &TautologyLaw.walk/1)
      assert result
    end

    test "applies tautology: (B OR NOT A) OR A" do
      expr = b(:b or not :a or :a)
      result = Expression.postwalk(expr, &TautologyLaw.walk/1)
      assert result
    end

    test "applies tautology: (A OR B) OR NOT A" do
      expr = b(:a or :b or not :a)
      result = Expression.postwalk(expr, &TautologyLaw.walk/1)
      assert result
    end

    test "applies tautology: NOT A OR (A OR B)" do
      expr = b(not :a or (:a or :b))
      result = Expression.postwalk(expr, &TautologyLaw.walk/1)
      assert result
    end

    test "applies tautology: NOT A OR (B OR A)" do
      expr = b(not :a or (:b or :a))
      result = Expression.postwalk(expr, &TautologyLaw.walk/1)
      assert result
    end

    test "works with complex sub-expressions" do
      # (X AND Y) OR (NOT (X AND Y) OR Z) = true
      complex_expr = b(:x and :y)
      expr = b(complex_expr or (not complex_expr or :z))
      result = Expression.postwalk(expr, &TautologyLaw.walk/1)
      assert result
    end

    test "works with negated complex sub-expressions" do
      # NOT (P OR Q) OR ((P OR Q) OR R) = true
      complex_expr = b(:p or :q)
      expr = b(not complex_expr or (complex_expr or :r))
      result = Expression.postwalk(expr, &TautologyLaw.walk/1)
      assert result
    end

    test "handles multiple tautology opportunities" do
      # (A OR (NOT A OR B)) AND (C OR (NOT C OR D)) = true AND true = true
      expr = b((:a or (not :a or :b)) and (:c or (not :c or :d)))
      result = Expression.postwalk(expr, &TautologyLaw.walk/1)
      assert result
    end

    test "works with deeply nested expressions" do
      # Tautology within a larger expression
      expr = b((:m and :n) or (:a or (not :a or :b)))
      result = Expression.postwalk(expr, &TautologyLaw.walk/1)
      assert result == b((:m and :n) or true)
    end

    test "handles tautologies with boolean constants" do
      # true OR (NOT true OR B) = true
      expr = b(true or (not true or :b))
      result = Expression.postwalk(expr, &TautologyLaw.walk/1)
      assert result
    end

    test "works with multiple variables in tautology" do
      # Different variable combinations
      expr = b(:x or (not :x or (:y and :z)))
      result = Expression.postwalk(expr, &TautologyLaw.walk/1)
      assert result
    end

    test "handles very complex identical sub-expressions" do
      complex_expr = b((:p and :q) or (:r and not :s))
      expr = b(complex_expr or (not complex_expr or :t))
      result = Expression.postwalk(expr, &TautologyLaw.walk/1)
      assert result
    end

    test "leaves non-tautology patterns unchanged" do
      # Missing the complement
      expr = b(:a or (:b or :c))
      assert Expression.postwalk(expr, &TautologyLaw.walk/1) == expr
    end

    test "leaves different variable patterns unchanged" do
      # Variables don't form tautology
      expr = b(:a or (not :b or :c))
      assert Expression.postwalk(expr, &TautologyLaw.walk/1) == expr
    end

    test "leaves AND operations unchanged" do
      # Tautology patterns only work with OR
      expr = b(:a and (not :a and :b))
      assert Expression.postwalk(expr, &TautologyLaw.walk/1) == expr
    end

    test "leaves single variables unchanged" do
      assert Expression.postwalk(:a, &TautologyLaw.walk/1) == :a
    end

    test "leaves simple expressions unchanged" do
      assert Expression.postwalk(b(:a and :b), &TautologyLaw.walk/1) == b(:a and :b)
      assert Expression.postwalk(b(:a or :b), &TautologyLaw.walk/1) == b(:a or :b)
    end

    test "leaves boolean constants unchanged" do
      assert Expression.postwalk(true, &TautologyLaw.walk/1)
      refute Expression.postwalk(false, &TautologyLaw.walk/1)
    end

    test "handles partial tautology patterns" do
      # Has complement but wrong structure
      expr = b(:a and (:b or not :a))
      assert Expression.postwalk(expr, &TautologyLaw.walk/1) == expr
    end

    test "preserves expressions without tautology patterns" do
      expr = b((:a and :b) or (:c and :d))
      assert Expression.postwalk(expr, &TautologyLaw.walk/1) == expr
    end

    test "works with symmetric tautology patterns" do
      # Multiple variables in symmetric positions
      expr = b(:x or (:y or not :x))
      result = Expression.postwalk(expr, &TautologyLaw.walk/1)
      assert result
    end

    test "handles nested tautologies" do
      # Tautology containing another tautology
      inner_tautology = b(:p or (not :p or :q))
      expr = b(:a or (not :a or inner_tautology))
      result = Expression.postwalk(expr, &TautologyLaw.walk/1)
      assert result
    end

    test "works with mixed tautology types" do
      # Different tautology patterns in one expression
      expr = b((:a or (not :a or :b)) and (:c or (:d or not :c)))
      result = Expression.postwalk(expr, &TautologyLaw.walk/1)
      assert result
    end
  end

  describe "edge cases" do
    test "handles all tautology ordering patterns" do
      # Test all 7 implemented patterns systematically
      tautology_patterns = [
        # A OR (NOT A OR B)
        b(:x or (not :x or :y)),
        # A OR (B OR NOT A)
        b(:x or (:y or not :x)),
        # (NOT A OR B) OR A
        b(not :x or :y or :x),
        # (B OR NOT A) OR A
        b(:y or not :x or :x),
        # (A OR B) OR NOT A
        b(:x or :y or not :x),
        # NOT A OR (A OR B)
        b(not :x or (:x or :y)),
        # NOT A OR (B OR A)
        b(not :x or (:y or :x))
      ]

      for expr <- tautology_patterns do
        result = Expression.postwalk(expr, &TautologyLaw.walk/1)
        assert result, "Pattern #{inspect(expr)} should be recognized as tautology"
      end
    end

    test "preserves non-tautology disjunctions" do
      # Valid OR patterns that are not tautologies
      non_tautologies = [
        b(:a or :b),
        b(:a or (:b or :c)),
        b(:a or :b or :c),
        b(:a or (not :b or :c))
      ]

      for expr <- non_tautologies do
        result = Expression.postwalk(expr, &TautologyLaw.walk/1)
        assert result == expr, "Pattern #{inspect(expr)} should remain unchanged"
      end
    end

    test "handles tautologies with boolean constants correctly" do
      # Patterns involving true/false
      patterns_and_expected = [
        {b(true or (not true or :x)), true},
        {b(false or (not false or :x)), true},
        {b(:x or (not :x or true)), true},
        {b(:x or (not :x or false)), true}
      ]

      for {expr, expected} <- patterns_and_expected do
        result = Expression.postwalk(expr, &TautologyLaw.walk/1)
        assert result == expected
      end
    end

    test "preserves complex non-tautology patterns" do
      # Complex expressions that shouldn't match
      expr = b((:a and :b) or (not (:c or :d) or :e))
      assert Expression.postwalk(expr, &TautologyLaw.walk/1) == expr
    end

    test "handles deeply nested tautology detection" do
      # Very nested structure with tautology
      expr = b((:m and :n) or ((:p or :q) and (:a or (not :a or :b))))
      result = Expression.postwalk(expr, &TautologyLaw.walk/1)
      assert result == b((:m and :n) or ((:p or :q) and true))
    end

    test "works with repeated variables in different roles" do
      # Same variable used multiple times
      expr = b(:a or (not :a or (:a and :b)))
      result = Expression.postwalk(expr, &TautologyLaw.walk/1)
      assert result
    end

    test "preserves AND operations with complement patterns" do
      # Should not apply to AND operations
      and_patterns = [
        b(:a and (not :a and :b)),
        b(not :a and :b and :a),
        b(:a and :b and not :a)
      ]

      for expr <- and_patterns do
        result = Expression.postwalk(expr, &TautologyLaw.walk/1)
        assert result == expr
      end
    end
  end

  property "applying tautology law is idempotent" do
    check all(expr <- Expression.generate_expression(StreamData.atom(:alphanumeric))) do
      result1 = Expression.postwalk(expr, &TautologyLaw.walk/1)
      result2 = Expression.postwalk(result1, &TautologyLaw.walk/1)
      assert result1 == result2
    end
  end

  property "tautology law preserves logical equivalence" do
    check all(
            assignments <-
              StreamData.map_of(StreamData.atom(:alphanumeric), StreamData.boolean(),
                min_length: 1
              ),
            variable_names = Map.keys(assignments),
            expr <- Expression.generate_expression(StreamData.member_of(variable_names))
          ) do
      result = Expression.postwalk(expr, &TautologyLaw.walk/1)
      eval_fn = &Map.fetch!(assignments, &1)

      assert Expression.run(expr, eval_fn) == Expression.run(result, eval_fn),
             """
             Tautology law changed the logical outcome!
             Original: #{inspect(expr, pretty: true)}
             Transformed: #{inspect(result, pretty: true)}
             Assignments: #{inspect(assignments, pretty: true)}
             """
    end
  end
end
