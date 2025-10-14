# SPDX-FileCopyrightText: 2020 Zach Daniel
#
# SPDX-License-Identifier: MIT

# credo:disable-for-this-file Credo.Check.Warning.BoolOperationOnSameValues
defmodule Ash.Test.SatSolver.Expression.RewriteRule.CommutativityLaw do
  use ExUnit.Case, async: true
  use ExUnitProperties

  import Ash.SatSolver.Expression, only: [b: 1]

  alias Ash.SatSolver.Expression
  alias Ash.SatSolver.Expression.RewriteRule.CommutativityLaw

  doctest CommutativityLaw, import: true

  describe inspect(&CommutativityLaw.walk/1) do
    test "sorts operands in AND expressions" do
      assert Expression.postwalk(b(:b and :a), &CommutativityLaw.walk/1) == b(:a and :b)
    end

    test "sorts operands in OR expressions" do
      assert Expression.postwalk(b(:z or :x), &CommutativityLaw.walk/1) == b(:x or :z)
    end

    test "handles complex expressions with sorting" do
      expr = b((:z and :y) or (:b and :a))
      result = Expression.postwalk(expr, &CommutativityLaw.walk/1)
      assert result == b((:a and :b) or (:y and :z))
    end

    test "sorts nested expressions recursively" do
      expr = b((:z or :x) and (:b or :a))
      result = Expression.postwalk(expr, &CommutativityLaw.walk/1)
      assert result == b((:a or :b) and (:x or :z))
    end

    test "handles deeply nested expressions" do
      expr = b(((:z and :y) or :x) and (:c or (:b and :a)))
      result = Expression.postwalk(expr, &CommutativityLaw.walk/1)
      assert result == b((:c or (:a and :b)) and (:x or (:y and :z)))
    end

    test "works with boolean constants" do
      assert Expression.postwalk(b(false and true), &CommutativityLaw.walk/1) == b(false and true)
      assert Expression.postwalk(b(true or false), &CommutativityLaw.walk/1) == b(false or true)
    end

    test "handles mixed boolean constants and variables" do
      expr = b((:b and true) or (false and :a))
      result = Expression.postwalk(expr, &CommutativityLaw.walk/1)
      assert result == b((:a and false) or (:b and true))
    end

    test "sorts complex nested structures" do
      expr = b((not (:z or :x) and :b) or (:a and not (:y or :w)))
      result = Expression.postwalk(expr, &CommutativityLaw.walk/1)
      expected = b((:a and not (:w or :y)) or (:b and not (:x or :z)))
      assert result == expected
    end

    test "leaves single variables unchanged" do
      assert Expression.postwalk(:a, &CommutativityLaw.walk/1) == :a
    end

    test "leaves simple negations unchanged" do
      assert Expression.postwalk(b(not :a), &CommutativityLaw.walk/1) == b(not :a)
    end

    test "leaves boolean constants unchanged" do
      assert Expression.postwalk(true, &CommutativityLaw.walk/1)
      refute Expression.postwalk(false, &CommutativityLaw.walk/1)
    end

    test "handles multiple levels of nesting with sorting" do
      expr = b((((:d or :c) and (:b or :a)) or :z) and :x)
      result = Expression.postwalk(expr, &CommutativityLaw.walk/1)
      expected = b(:x and (:z or ((:a or :b) and (:c or :d))))
      assert result == expected
    end

    test "works with symmetric expressions" do
      expr = b((:b and :a) or (:a and :b))
      result = Expression.postwalk(expr, &CommutativityLaw.walk/1)
      assert result == b((:a and :b) or (:a and :b))
    end

    test "handles alternating operators with sorting" do
      expr = b((:z or (:y and :x)) and (:c or (:b and :a)))
      result = Expression.postwalk(expr, &CommutativityLaw.walk/1)
      expected = b((:c or (:a and :b)) and (:z or (:x and :y)))
      assert result == expected
    end

    test "preserves expression structure while sorting" do
      expr = b((:d and (:c or :b)) or (:a and (:z or :y)))
      result = Expression.postwalk(expr, &CommutativityLaw.walk/1)
      expected = b((:a and (:y or :z)) or (:d and (:b or :c)))
      assert result == expected
    end

    test "sorts identical variables consistently" do
      expr = b((:a and :a) or (:a and :a))
      result = Expression.postwalk(expr, &CommutativityLaw.walk/1)
      assert result == b((:a and :a) or (:a and :a))
    end

    test "handles very complex nested sorting" do
      expr = b(((:w or :v) and (:u or :t)) or ((:s or :r) and (:q or :p)))
      result = Expression.postwalk(expr, &CommutativityLaw.walk/1)
      expected = b(((:p or :q) and (:r or :s)) or ((:t or :u) and (:v or :w)))
      assert result == expected
    end

    test "works with negated complex expressions" do
      expr = b(not (:z and :y) or not (:x and :w))
      result = Expression.postwalk(expr, &CommutativityLaw.walk/1)
      expected = b(not (:w and :x) or not (:y and :z))
      assert result == expected
    end

    test "preserves non-sortable patterns" do
      expr = b(not (not :a))
      result = Expression.postwalk(expr, &CommutativityLaw.walk/1)
      assert result == b(not (not :a))
    end

    test "handles triple nested expressions" do
      expr = b(((:c or :b) and :a) or ((:f or :e) and :d))
      result = Expression.postwalk(expr, &CommutativityLaw.walk/1)
      expected = b((:a and (:b or :c)) or (:d and (:e or :f)))
      assert result == expected
    end

    test "sorts with very long expressions" do
      expr = b((:h or :g) and (:f or :e) and ((:d or :c) and (:b or :a)))
      result = Expression.postwalk(expr, &CommutativityLaw.walk/1)
      expected = b((:a or :b) and (:c or :d) and ((:e or :f) and (:g or :h)))
      assert result == expected
    end
  end

  describe "edge cases" do
    test "handles all balance patterns" do
      exprs_and_expected = [
        {b(:b and :a), b(:a and :b)},
        {b(:z or :x), b(:x or :z)},
        {b((:y and :x) or (:b and :a)), b((:a and :b) or (:x and :y))},
        {b((:z or :y) and (:x or :w)), b((:w or :x) and (:y or :z))}
      ]

      for {expr, expected} <- exprs_and_expected do
        assert Expression.postwalk(expr, &CommutativityLaw.walk/1) == expected
      end
    end

    test "works with deeply nested balance requirements" do
      expr = b((((:e or :d) and (:c or :b)) or :a) and :z and :y)
      result = Expression.postwalk(expr, &CommutativityLaw.walk/1)
      expected = b(:y and (:z and (:a or ((:b or :c) and (:d or :e)))))
      assert result == expected
    end

    test "preserves non-balance complex patterns" do
      expr = b(not (:a and :b) and not (:c or :d))
      result = Expression.postwalk(expr, &CommutativityLaw.walk/1)
      expected = b(not (:a and :b) and not (:c or :d))
      assert result == expected
    end

    test "handles mixed balance and non-balance" do
      expr = b(:b and :a and not (:c or :d))
      result = Expression.postwalk(expr, &CommutativityLaw.walk/1)
      expected = b(not (:c or :d) and (:a and :b))
      assert result == expected
    end

    test "works with complex mixed operators" do
      expr = b(((:d or :c) and (:b and :a)) or ((:h or :g) and (:f and :e)))
      result = Expression.postwalk(expr, &CommutativityLaw.walk/1)
      expected = b((:a and :b and (:c or :d)) or (:e and :f and (:g or :h)))
      assert result == expected
    end
  end

  property "applying balance law is idempotent" do
    check all(expr <- Expression.generate_expression(StreamData.atom(:alphanumeric))) do
      result1 = Expression.postwalk(expr, &CommutativityLaw.walk/1)
      result2 = Expression.postwalk(result1, &CommutativityLaw.walk/1)
      assert result1 == result2
    end
  end

  property "balance law preserves logical equivalence" do
    check all(
            assignments <-
              StreamData.map_of(StreamData.atom(:alphanumeric), StreamData.boolean(),
                min_length: 1
              ),
            variable_names = Map.keys(assignments),
            expr <- Expression.generate_expression(StreamData.member_of(variable_names))
          ) do
      result = Expression.postwalk(expr, &CommutativityLaw.walk/1)
      eval_fn = &Map.fetch!(assignments, &1)

      assert Expression.run(expr, eval_fn) == Expression.run(result, eval_fn),
             """
             Balance law changed the logical outcome!
             Original: #{inspect(expr, pretty: true)}
             Transformed: #{inspect(result, pretty: true)}
             Assignments: #{inspect(assignments, pretty: true)}
             """
    end
  end
end
