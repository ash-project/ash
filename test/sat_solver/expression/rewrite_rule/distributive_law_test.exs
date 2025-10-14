# SPDX-FileCopyrightText: ash contributors <https://github.com/ash-project/ash/graphs.contributors>
#
#
# SPDX-License-Identifier: MIT

defmodule Ash.Test.SatSolver.Expression.RewriteRule.DistributiveLaw do
  use ExUnit.Case, async: true
  use ExUnitProperties

  import Ash.SatSolver.Expression, only: [b: 1]

  alias Ash.SatSolver.Expression
  alias Ash.SatSolver.Expression.RewriteRule
  alias Ash.SatSolver.Expression.RewriteRule.DistributiveLaw

  doctest DistributiveLaw, import: true

  describe inspect(&DistributiveLaw.walk/1) do
    test "distributes A OR (B AND C)" do
      expr = b(:a or (:b and :c))
      {result, _acc_map} = RewriteRule.apply(expr, [DistributiveLaw])
      assert result == b((:a or :b) and (:a or :c))
    end

    test "distributes (A AND B) OR C" do
      expr = b((:a and :b) or :c)
      {result, _acc_map} = RewriteRule.apply(expr, [DistributiveLaw])
      assert result == b((:a or :c) and (:b or :c))
    end

    test "handles complex nested distributions requiring reapplication" do
      expr = b(:a or ((:b and :c) or (:d and :e)))
      {result, _acc_map} = RewriteRule.apply(expr, [DistributiveLaw])
      # The actual result shows the structure the distributive law produces
      expected =
        b(
          (:a or (:b or :d)) and (:a or (:c or :d)) and
            ((:a or (:b or :e)) and (:a or (:c or :e)))
        )

      assert result == expected
    end

    test "leaves non-matching patterns unchanged" do
      expr = b(:a and (:b or :c))
      {result, _acc_map} = RewriteRule.apply(expr, [DistributiveLaw])
      assert result == b(:a and (:b or :c))
    end

    test "handles multiple distributive opportunities" do
      expr = b((:a or (:b and :c)) and (:d or (:e and :f)))
      {result, _acc_map} = RewriteRule.apply(expr, [DistributiveLaw])
      assert result == b((:a or :b) and (:a or :c) and ((:d or :e) and (:d or :f)))
    end

    test "works with deeply nested expressions" do
      expr = b(:x or (:y and (:z or (:w and :v))))
      {result, _acc_map} = RewriteRule.apply(expr, [DistributiveLaw])
      # Should distribute :x over the AND, then continue distributing within
      expected = b((:x or :y) and ((:x or (:z or :w)) and (:x or (:z or :v))))
      assert result == expected
    end

    test "distributes with boolean constants" do
      expr = b(true or (false and :a))
      {result, _acc_map} = RewriteRule.apply(expr, [DistributiveLaw])
      assert result == b((true or false) and (true or :a))
    end

    test "handles symmetric distribution patterns" do
      expr = b((:x and :y) or (:z and :w))
      {result, _acc_map} = RewriteRule.apply(expr, [DistributiveLaw])
      # The actual distribution creates this nested AND structure
      expected = b((:x or :z) and (:y or :z) and ((:x or :w) and (:y or :w)))
      assert result == expected
    end

    test "applies to nested AND structures" do
      expr = b(:a or (:b and :c and (:d and :e)))
      {result, _acc_map} = RewriteRule.apply(expr, [DistributiveLaw])
      # Should distribute :a over the outer AND
      expected = b((:a or :b) and (:a or :c) and ((:a or :d) and (:a or :e)))
      assert result == expected
    end

    test "preserves expressions that don't need distribution" do
      expr = b((:a or :b) and (:c or :d))
      {result, _acc_map} = RewriteRule.apply(expr, [DistributiveLaw])
      assert result == b((:a or :b) and (:c or :d))
    end

    test "handles single variable cases" do
      expr = b(:a or :b)
      {result, _acc_map} = RewriteRule.apply(expr, [DistributiveLaw])
      assert result == b(:a or :b)
    end

    test "distributes complex nested OR over AND" do
      expr = b(:a or :b or :c or (:d and :e))
      {result, _acc_map} = RewriteRule.apply(expr, [DistributiveLaw])
      assert result == b((:a or :b or :c or :d) and (:a or :b or :c or :e))
    end
  end

  describe "reapplication behavior" do
    test "continues applying until CNF is achieved" do
      expr = b(:a or (:b or (:c and :d)))
      {result, _acc_map} = RewriteRule.apply(expr, [DistributiveLaw])
      # Should keep distributing until all OR-over-AND patterns are resolved
      expected = b((:a or (:b or :c)) and (:a or (:b or :d)))
      assert result == expected
    end

    test "handles deeply nested distributions" do
      expr = b(:w or (:x or (:y and (:z1 and :z2))))
      {result, _acc_map} = RewriteRule.apply(expr, [DistributiveLaw])
      # Should distribute at multiple levels
      expected = b((:w or (:x or :y)) and ((:w or (:x or :z1)) and (:w or (:x or :z2))))
      assert result == expected
    end

    test "converts complex expressions to CNF form" do
      expr = b(:a or (:b and :c) or (:d and (:e or :f)))
      {result, _acc_map} = RewriteRule.apply(expr, [DistributiveLaw])
      # Should result in a conjunction of disjunctions (CNF)
      # This is a complex case - the exact result depends on order of application
      # but should be in CNF form (conjunction of disjunctions)
      assert Expression.is_in_cnf?(result)
    end
  end

  describe "CNF conversion properties" do
    test "produces CNF for simple cases" do
      expr = b(:a or (:b and :c))
      {result, _acc_map} = RewriteRule.apply(expr, [DistributiveLaw])
      assert Expression.is_in_cnf?(result)
    end

    test "maintains CNF for already CNF expressions" do
      expr = b((:a or :b) and (:c or :d))
      {result, _acc_map} = RewriteRule.apply(expr, [DistributiveLaw])
      assert Expression.is_in_cnf?(result)
      assert result == expr
    end
  end

  property "applying distributive law is idempotent after first application" do
    check all(expr <- Expression.generate_expression(StreamData.atom(:alphanumeric))) do
      {result1, _acc_map1} = RewriteRule.apply(expr, [DistributiveLaw])
      {result2, _acc_map2} = RewriteRule.apply(result1, [DistributiveLaw])
      assert result1 == result2
    end
  end

  property "distributive law preserves logical equivalence" do
    check all(
            assignments <-
              StreamData.map_of(StreamData.atom(:alphanumeric), StreamData.boolean(),
                min_length: 1
              ),
            variable_names = Map.keys(assignments),
            expr <- Expression.generate_expression(StreamData.member_of(variable_names))
          ) do
      {result, _acc_map} = RewriteRule.apply(expr, [DistributiveLaw])
      eval_fn = &Map.fetch!(assignments, &1)

      assert Expression.run(expr, eval_fn) == Expression.run(result, eval_fn),
             """
             Distributive law changed the logical outcome!
             Original: #{inspect(expr, pretty: true)}
             Transformed: #{inspect(result, pretty: true)}
             Assignments: #{inspect(assignments, pretty: true)}
             """
    end
  end
end
