# SPDX-FileCopyrightText: 2020 Zach Daniel
#
# SPDX-License-Identifier: MIT

defmodule Ash.Test.SatSolver.Expression.RewriteRule.AnnihilatorLaw do
  use ExUnit.Case, async: true
  use ExUnitProperties

  import Ash.SatSolver.Expression, only: [b: 1]

  alias Ash.SatSolver.Expression
  alias Ash.SatSolver.Expression.RewriteRule.AnnihilatorLaw

  doctest AnnihilatorLaw, import: true

  describe inspect(&AnnihilatorLaw.walk/1) do
    test "applies AND annihilator: A AND false" do
      refute Expression.postwalk(b(:a and false), &AnnihilatorLaw.walk/1)
    end

    test "applies AND annihilator: false AND A" do
      refute Expression.postwalk(b(false and :a), &AnnihilatorLaw.walk/1)
    end

    test "applies OR annihilator: A OR true" do
      assert Expression.postwalk(b(:a or true), &AnnihilatorLaw.walk/1)
    end

    test "applies OR annihilator: true OR A" do
      assert Expression.postwalk(b(true or :a), &AnnihilatorLaw.walk/1)
    end

    test "works with complex sub-expressions" do
      expr = b(:a and :b and false)
      result = Expression.postwalk(expr, &AnnihilatorLaw.walk/1)
      refute result
    end

    test "works with complex tautology patterns" do
      expr = b(:x or :y or true)
      result = Expression.postwalk(expr, &AnnihilatorLaw.walk/1)
      assert result
    end

    test "handles nested annihilators" do
      expr = b((:a or true) and :b)
      result = Expression.postwalk(expr, &AnnihilatorLaw.walk/1)
      assert result == b(true and :b)
    end

    test "works with deeply nested expressions" do
      expr = b(:p and :q and (false and :r))
      result = Expression.postwalk(expr, &AnnihilatorLaw.walk/1)
      refute result
    end

    test "handles multiple variables in complex expressions" do
      expr = b(((:a and :b) or true) and :c)
      result = Expression.postwalk(expr, &AnnihilatorLaw.walk/1)
      assert result == b(true and :c)
    end

    test "leaves non-matching patterns unchanged" do
      assert Expression.postwalk(b(:a and :b), &AnnihilatorLaw.walk/1) == b(:a and :b)
      assert Expression.postwalk(b(:a or :b), &AnnihilatorLaw.walk/1) == b(:a or :b)
      assert Expression.postwalk(b(:a and true), &AnnihilatorLaw.walk/1) == b(:a and true)
      assert Expression.postwalk(b(:a or false), &AnnihilatorLaw.walk/1) == b(:a or false)
    end

    test "leaves single variables unchanged" do
      assert Expression.postwalk(:a, &AnnihilatorLaw.walk/1) == :a
    end

    test "leaves boolean constants unchanged" do
      assert Expression.postwalk(true, &AnnihilatorLaw.walk/1)
      refute Expression.postwalk(false, &AnnihilatorLaw.walk/1)
    end

    test "handles multiple annihilator opportunities" do
      expr = b((false and :a) or (true or :b))
      result = Expression.postwalk(expr, &AnnihilatorLaw.walk/1)
      assert result
    end

    test "works in complex mixed expressions" do
      expr = b((false and :x) or (true or :y))
      result = Expression.postwalk(expr, &AnnihilatorLaw.walk/1)
      assert result
    end
  end

  describe "edge cases" do
    test "handles complex boolean structures" do
      expr = b((:a or :b) and false and (:c and :d))
      result = Expression.postwalk(expr, &AnnihilatorLaw.walk/1)
      refute result
    end

    test "preserves non-annihilator patterns" do
      expr = b((:a and :b) or (:c and :d))
      result = Expression.postwalk(expr, &AnnihilatorLaw.walk/1)
      assert result == b((:a and :b) or (:c and :d))
    end
  end

  property "applying annihilator law is idempotent" do
    check all(expr <- Expression.generate_expression(StreamData.atom(:alphanumeric))) do
      result1 = Expression.postwalk(expr, &AnnihilatorLaw.walk/1)
      result2 = Expression.postwalk(result1, &AnnihilatorLaw.walk/1)
      assert result1 == result2
    end
  end

  property "annihilator laws preserve logical equivalence" do
    check all(
            assignments <-
              StreamData.map_of(StreamData.atom(:alphanumeric), StreamData.boolean(),
                min_length: 1
              ),
            variable_names = Map.keys(assignments),
            expr <- Expression.generate_expression(StreamData.member_of(variable_names))
          ) do
      result = Expression.postwalk(expr, &AnnihilatorLaw.walk/1)
      eval_fn = &Map.fetch!(assignments, &1)

      assert Expression.run(expr, eval_fn) == Expression.run(result, eval_fn),
             """
             Annihilator law changed the logical outcome!
             Original: #{inspect(expr, pretty: true)}
             Transformed: #{inspect(result, pretty: true)}
             Assignments: #{inspect(assignments, pretty: true)}
             """
    end
  end
end
