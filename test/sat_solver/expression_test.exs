# SPDX-FileCopyrightText: ash contributors <https://github.com/ash-project/ash/graphs.contributors>
#
#
# SPDX-License-Identifier: MIT

# credo:disable-for-this-file Credo.Check.Warning.BoolOperationOnSameValues
defmodule Ash.Test.SatSolver.Expression do
  use ExUnit.Case, async: true
  use ExUnitProperties

  import Ash.SatSolver.Expression, only: [b: 1]

  alias Ash.SatSolver.Expression

  doctest Expression, import: true

  describe inspect(&Expression.b/1) do
    test "creates and expressions" do
      assert {:and, :a, :b} = b(:a and :b)
    end

    test "creates or expressions" do
      assert {:or, :a, :b} = b(:a or :b)
    end

    test "creates not expressions" do
      assert {:not, :a} = b(not :a)
    end

    test "creates nand expressions" do
      assert b(not (:a and :b)) = b(nand(:a, :b))
    end

    test "creates nor expressions" do
      assert b(not (:a or :b)) = b(nor(:a, :b))
    end

    test "creates xor expressions" do
      assert b((:a or :b) and not (:a and :b)) = b(xor(:a, :b))
    end

    test "creates xnor expressions" do
      assert b(not ((:a or :b) and not (:a and :b))) = b(xnor(:a, :b))
    end

    test "creates implies expressions" do
      assert b(not (:a and not :b)) = b(implies(:a, :b))
    end

    test "creates implied_by expressions" do
      assert b(not (:b and not :a)) = b(implied_by(:a, :b))
    end

    test "nests expressions" do
      assert {:and, {:or, :a, :b}, {:not, :c}} = b((:a or :b) and not :c)
    end

    test "nests nand expressions" do
      assert {:or, {:not, {:and, :a, :b}}, :c} = b(nand(:a, :b) or :c)
    end
  end

  describe inspect(&Expression.to_ast/1) do
    test "converts and expressions" do
      assert {:and, _meta, [:a, :b]} = Expression.to_ast(b(:a and :b))
    end

    test "converts or expressions" do
      assert {:or, _meta, [:a, :b]} = Expression.to_ast(b(:a or :b))
    end

    test "converts not expressions" do
      assert {:not, _meta, [:a]} = Expression.to_ast(b(not :a))
    end

    test "converts nand expressions" do
      assert {:not, _meta, [{:and, _meta2, [:a, :b]}]} = Expression.to_ast(b(nand(:a, :b)))
    end

    test "nests expressions" do
      assert {:and, _, [{:or, _, [:a, :b]}, {:not, _, [:c]}]} =
               Expression.to_ast(b((:a or :b) and not :c))
    end

    test "allows customizing variable representation" do
      assert {:and, _, [{:a, _, []}, {:b, _, []}]} =
               Expression.to_ast(b(:a and :b), &var_as_fun/1)
    end
  end

  describe inspect(&Expression.to_algebra/3) do
    test "converts simple expressions to algebra" do
      assert ":a and :b" =
               b(:a and :b)
               |> Expression.to_algebra()
               |> Inspect.Algebra.format(80)
               |> IO.iodata_to_binary()
    end

    test "converts complex expressions with proper precedence" do
      assert "(:a or :b) and not :c" =
               b((:a or :b) and not :c)
               |> Expression.to_algebra()
               |> Inspect.Algebra.format(80)
               |> IO.iodata_to_binary()
    end

    test "allows customizing Code.quoted_to_algebra options" do
      assert "a and b()" =
               b({:a, [], Elixir} and {:b, [], []})
               |> Expression.to_algebra([locals_without_parens: [a: 0]], & &1)
               |> Inspect.Algebra.format(80)
               |> IO.iodata_to_binary()
    end
  end

  describe inspect(&Expression.to_string/2) do
    test "converts simple expressions to strings" do
      assert ":a and :b" = Expression.to_string(b(:a and :b))
      assert ":a or :b" = Expression.to_string(b(:a or :b))
      assert "not :a" = Expression.to_string(b(not :a))
    end

    test "converts nested expressions to strings with proper precedence" do
      assert "(:a or :b) and not :c" = Expression.to_string(b((:a or :b) and not :c))
      assert "(:a and :b) or not :c" = Expression.to_string(b((:a and :b) or not :c))
      assert "not (:a and :b)" = Expression.to_string(b(not (:a and :b)))
      assert "not (:a or :b)" = Expression.to_string(b(not (:a or :b)))
      assert "not not :a" = Expression.to_string(b(not not :a))
    end

    test "handles single variable expressions without parentheses" do
      assert ":a" = Expression.to_string(b(:a))
    end

    test "handles boolean literals without parentheses" do
      assert "true" = Expression.to_string(b(true))
      assert "false" = Expression.to_string(b(false))
    end

    test "allows customizing variable representation" do
      assert "a() and b()" = Expression.to_string(b(:a and :b), &var_as_fun/1)
    end
  end

  describe inspect(&Expression.prewalk/2) do
    test "transforms and to or" do
      assert b(:a or :b) =
               Expression.prewalk(b(:a and :b), fn
                 b(left and right) -> b(left or right)
                 other -> other
               end)
    end

    test "transforms nested expressions" do
      assert b(:a or (:b and :c)) =
               Expression.prewalk(b(:a and (:b or :c)), fn
                 b(left and right) -> b(left or right)
                 b(left or right) -> b(left and right)
                 other -> other
               end)
    end

    test "transforms variables" do
      assert b({:var, :a} and {:var, :b}) =
               Expression.prewalk(b(:a and :b), fn
                 var when is_atom(var) -> {:var, var}
                 other -> other
               end)
    end

    test "handles boolean literals" do
      assert b(false and :a) =
               Expression.prewalk(b(true and :a), fn
                 true -> false
                 other -> other
               end)
    end
  end

  describe inspect(&Expression.prewalk/3) do
    test "counts operators" do
      {_result, count} =
        Expression.prewalk(b(:a and (:b or :c)), 0, fn
          b(_left and _right) = expr, acc -> {expr, acc + 1}
          b(_left or _right) = expr, acc -> {expr, acc + 1}
          other, acc -> {other, acc}
        end)

      assert count == 2
    end

    test "collects variables" do
      {_result, vars} =
        Expression.prewalk(b(:a and (:b or :c)), [], fn
          var, acc when is_atom(var) -> {var, [var | acc]}
          other, acc -> {other, acc}
        end)

      assert Enum.sort(vars) == [:a, :b, :c]
    end

    test "transforms and collects" do
      {result, ops} =
        Expression.prewalk(b(:a and :b), [], fn
          b(left and right), acc -> {b(left or right), [:and | acc]}
          other, acc -> {other, acc}
        end)

      assert result == b(:a or :b)
      assert ops == [:and]
    end
  end

  describe inspect(&Expression.postwalk/2) do
    test "transforms and to or" do
      assert b(:a or :b) =
               Expression.postwalk(b(:a and :b), fn
                 b(left and right) -> b(left or right)
                 other -> other
               end)
    end

    test "transforms nested expressions" do
      assert b(:a or (:b and :c)) =
               Expression.postwalk(b(:a and (:b or :c)), fn
                 b(left and right) -> b(left or right)
                 b(left or right) -> b(left and right)
                 other -> other
               end)
    end

    test "transforms variables" do
      assert b({:var, :a} and {:var, :b}) =
               Expression.postwalk(b(:a and :b), fn
                 var when is_atom(var) -> {:var, var}
                 other -> other
               end)
    end

    test "handles boolean literals" do
      assert b(false and :a) =
               Expression.postwalk(b(true and :a), fn
                 true -> false
                 other -> other
               end)
    end
  end

  describe inspect(&Expression.postwalk/3) do
    test "counts operators" do
      {_result, count} =
        Expression.postwalk(b(:a and (:b or :c)), 0, fn
          b(_left and _right) = expr, acc -> {expr, acc + 1}
          b(_left or _right) = expr, acc -> {expr, acc + 1}
          other, acc -> {other, acc}
        end)

      assert count == 2
    end

    test "collects variables" do
      {_result, vars} =
        Expression.postwalk(b(:a and (:b or :c)), [], fn
          var, acc when is_atom(var) -> {var, [var | acc]}
          other, acc -> {other, acc}
        end)

      assert Enum.sort(vars) == [:a, :b, :c]
    end

    test "transforms and collects" do
      {result, ops} =
        Expression.postwalk(b(:a and :b), [], fn
          b(left and right), acc -> {b(left or right), [:and | acc]}
          other, acc -> {other, acc}
        end)

      assert result == b(:a or :b)
      assert ops == [:and]
    end
  end

  describe inspect(&Expression.is_in_cnf?/1) do
    test "recognizes valid CNF" do
      assert Expression.is_in_cnf?(b(:a))
      assert Expression.is_in_cnf?(b(not :a))
      assert Expression.is_in_cnf?(b(:a or :b))
      assert Expression.is_in_cnf?(b(:a and :b))
      assert Expression.is_in_cnf?(b((:a or :b) and :c))
      assert Expression.is_in_cnf?(b((:a or :b) and (:c or :d)))
      assert Expression.is_in_cnf?(b(not :a or :b))
      assert Expression.is_in_cnf?(true)
      assert Expression.is_in_cnf?(false)
    end

    test "rejects invalid CNF" do
      refute Expression.is_in_cnf?(b(:a or (:b and :c)))
      refute Expression.is_in_cnf?(b(not (:a and :b)))
      refute Expression.is_in_cnf?(b(not (:a or :b)))
      refute Expression.is_in_cnf?(b(not not :a))
    end
  end

  describe inspect(&Expression.to_cnf/1) do
    test "simple expression already in CNF" do
      assert b(:a and :b) = Expression.to_cnf(b(:a and :b))
      assert b(:a or :b) = Expression.to_cnf(b(:a or :b))
    end

    test "converts OR of ANDs to CNF" do
      assert b((:a or :b) and (:a or :c)) = Expression.to_cnf(b(:a or (:b and :c)))
      assert b((:a or :c) and (:b or :c)) = Expression.to_cnf(b((:a and :b) or :c))
    end

    test "handles De Morgan's laws" do
      assert b(not :a or not :b) = Expression.to_cnf(b(not (:a and :b)))
      assert b(not :a and not :b) = Expression.to_cnf(b(not (:a or :b)))
    end

    test "eliminates double negation" do
      assert :a = Expression.to_cnf(b(not not :a))
    end

    test "complex expression to CNF" do
      # (A OR B) AND NOT(C AND D) should become (A OR B) AND (NOT C OR NOT D)
      result = Expression.to_cnf(b((:a or :b) and not (:c and :d)))
      assert b((:a or :b) and (not :c or not :d)) = result
    end

    property "to_cnf produces valid CNF" do
      check all(expr <- Expression.generate_expression(StreamData.atom(:alphanumeric))) do
        cnf = Expression.to_cnf(expr)

        assert Expression.is_in_cnf?(cnf),
               """
               Result is not in CNF!
               Original: #{inspect(expr, pretty: true)}
               Result: #{inspect(cnf, pretty: true)}
               """
      end
    end

    property "to_cnf preserves logical equivalence" do
      check all(
              assignments <-
                StreamData.map_of(StreamData.atom(:alphanumeric), StreamData.boolean(),
                  min_length: 1
                ),
              variable_names = Map.keys(assignments),
              expr <- Expression.generate_expression(StreamData.member_of(variable_names))
            ) do
        cnf = Expression.to_cnf(expr)
        eval_fn = &Map.fetch!(assignments, &1)

        assert Expression.run(expr, eval_fn) == Expression.run(cnf, eval_fn),
               """
               CNF conversion changed the logical outcome!
               Original: #{inspect(expr, pretty: true)}
               CNF: #{inspect(cnf, pretty: true)}
               Assignments: #{inspect(assignments, pretty: true)}
               """
      end
    end
  end

  describe inspect(&Expression.run/2) do
    test "evaluates and expressions" do
      assert Expression.run(b(:a and :b), &%{a: true, b: true}[&1])
      refute Expression.run(b(:a and :b), &%{a: true, b: false}[&1])
      refute Expression.run(b(:a and :b), &%{a: false, b: true}[&1])
      refute Expression.run(b(:a and :b), &%{a: false, b: false}[&1])
    end

    test "evaluates or expressions" do
      assert Expression.run(b(:a or :b), &%{a: true, b: true}[&1])
      assert Expression.run(b(:a or :b), &%{a: true, b: false}[&1])
      assert Expression.run(b(:a or :b), &%{a: false, b: true}[&1])
      refute Expression.run(b(:a or :b), &%{a: false, b: false}[&1])
    end

    test "evaluates not expressions" do
      refute Expression.run(b(not :a), &%{a: true}[&1])
      assert Expression.run(b(not :a), &%{a: false}[&1])
    end

    test "evaluates nand expressions" do
      refute Expression.run(b(nand(:a, :b)), &%{a: true, b: true}[&1])
      assert Expression.run(b(nand(:a, :b)), &%{a: true, b: false}[&1])
      assert Expression.run(b(nand(:a, :b)), &%{a: false, b: true}[&1])
      assert Expression.run(b(nand(:a, :b)), &%{a: false, b: false}[&1])
    end

    test "evaluates nor expressions" do
      refute Expression.run(b(nor(:a, :b)), &%{a: true, b: true}[&1])
      refute Expression.run(b(nor(:a, :b)), &%{a: true, b: false}[&1])
      refute Expression.run(b(nor(:a, :b)), &%{a: false, b: true}[&1])
      assert Expression.run(b(nor(:a, :b)), &%{a: false, b: false}[&1])
    end

    test "evaluates xor expressions" do
      refute Expression.run(b(xor(:a, :b)), &%{a: true, b: true}[&1])
      assert Expression.run(b(xor(:a, :b)), &%{a: true, b: false}[&1])
      assert Expression.run(b(xor(:a, :b)), &%{a: false, b: true}[&1])
      refute Expression.run(b(xor(:a, :b)), &%{a: false, b: false}[&1])
    end

    test "evaluates xnor expressions" do
      assert Expression.run(b(xnor(:a, :b)), &%{a: true, b: true}[&1])
      refute Expression.run(b(xnor(:a, :b)), &%{a: true, b: false}[&1])
      refute Expression.run(b(xnor(:a, :b)), &%{a: false, b: true}[&1])
      assert Expression.run(b(xnor(:a, :b)), &%{a: false, b: false}[&1])
    end

    test "evaluates implies expressions" do
      assert Expression.run(b(implies(:a, :b)), &%{a: true, b: true}[&1])
      refute Expression.run(b(implies(:a, :b)), &%{a: true, b: false}[&1])
      assert Expression.run(b(implies(:a, :b)), &%{a: false, b: true}[&1])
      assert Expression.run(b(implies(:a, :b)), &%{a: false, b: false}[&1])
    end

    test "evaluates implied_by expressions" do
      assert Expression.run(b(implied_by(:a, :b)), &%{a: true, b: true}[&1])
      assert Expression.run(b(implied_by(:a, :b)), &%{a: true, b: false}[&1])
      refute Expression.run(b(implied_by(:a, :b)), &%{a: false, b: true}[&1])
      assert Expression.run(b(implied_by(:a, :b)), &%{a: false, b: false}[&1])
    end

    test "evaluates nested expressions" do
      eval = &%{a: true, b: false, c: false}[&1]
      assert Expression.run(b((:a or :b) and not :c), eval)

      eval = &%{a: true, b: false, c: true}[&1]
      refute Expression.run(b((:a or :b) and not :c), eval)

      eval = &%{a: false, b: false, c: false}[&1]
      refute Expression.run(b((:a or :b) and not :c), eval)
    end

    test "evaluates boolean literals" do
      assert Expression.run(b(true), fn _ -> false end)
      refute Expression.run(b(false), fn _ -> true end)
      assert Expression.run(b(true and :a), &%{a: true}[&1])
      assert Expression.run(b(false or :a), &%{a: true}[&1])
    end

    test "evaluates complex nested expressions" do
      expr = b(((:a and :b) or :c) and not (:d and :e))

      eval = &%{a: true, b: true, c: false, d: true, e: true}[&1]
      refute Expression.run(expr, eval)

      eval = &%{a: true, b: true, c: false, d: false, e: true}[&1]
      assert Expression.run(expr, eval)

      eval = &%{a: false, b: false, c: true, d: true, e: false}[&1]
      assert Expression.run(expr, eval)
    end

    test "evaluates with custom callback functions" do
      refute Expression.run(b(:a and :b), fn
               :a -> true
               :b -> false
             end)

      assert Expression.run(b(:x or :y), fn var -> var == :x end)
    end
  end

  describe inspect(&Expression.simplify/1) do
    test "simplifies boolean expressions using all simplification laws" do
      expr = b((not not :a and (:a and :a)) or (true and false))
      result = Expression.simplify(expr)
      assert result == :a
    end
  end

  describe inspect(&Expression.balance/1) do
    test "balances boolean expressions by sorting operands" do
      assert Expression.balance(b(:b and :a)) == b(:a and :b)
      assert Expression.balance(b(:z or :x)) == b(:x or :z)
      assert Expression.balance(b((:z and :y) or (:b and :a))) == b((:a and :b) or (:y and :z))
    end
  end

  property "simplify is idempotent" do
    check all(expr <- Expression.generate_expression(StreamData.atom(:alphanumeric))) do
      result1 = Expression.simplify(expr)
      result2 = Expression.simplify(result1)
      assert result1 == result2
    end
  end

  property "simplify preserves logical equivalence" do
    check all(
            assignments <-
              StreamData.map_of(StreamData.atom(:alphanumeric), StreamData.boolean(),
                min_length: 1
              ),
            variable_names = Map.keys(assignments),
            expr <- Expression.generate_expression(StreamData.member_of(variable_names))
          ) do
      result = Expression.simplify(expr)
      eval_fn = &Map.fetch!(assignments, &1)

      assert Expression.run(expr, eval_fn) == Expression.run(result, eval_fn),
             """
             Simplify changed the logical outcome!
             Original: #{inspect(expr, pretty: true)}
             Simplified: #{inspect(result, pretty: true)}
             Assignments: #{inspect(assignments, pretty: true)}
             """
    end
  end

  describe inspect(&Expression.expand/3) do
    test "short-circuits on true in or" do
      expr = b(true or :never_evaluated)

      {result, visited} =
        Expression.expand(expr, [], fn node, visited ->
          {node, [node | visited]}
        end)

      assert result
      # Should not visit :never_evaluated
      refute :never_evaluated in visited
    end

    test "short-circuits on false in and" do
      expr = b(false and :never_evaluated)

      {result, visited} =
        Expression.expand(expr, [], fn node, visited ->
          {node, [node | visited]}
        end)

      refute result
      # Should not visit :never_evaluated
      refute :never_evaluated in visited
    end

    test "evaluates both sides when no short-circuit" do
      expr = b(:a and :b)

      {result, visited} =
        Expression.expand(expr, [], fn node, visited ->
          {node, [node | visited]}
        end)

      assert result == expr
      assert :a in visited
      assert :b in visited
    end

    test "simplifies not true immediately" do
      expr = b(not true)

      result = Expression.expand(expr, fn node -> node end)

      refute result
    end

    test "simplifies not false immediately" do
      expr = b(not false)

      result = Expression.expand(expr, fn node -> node end)

      assert result
    end
  end

  describe inspect(&Expression.at_most_one/1) do
    test "returns true for empty input" do
      assert Expression.at_most_one([]) == true
    end

    test "returns true for single variable" do
      assert Expression.at_most_one([:a]) == true
    end

    test "creates nand constraint for two variables" do
      result = Expression.at_most_one([:a, :b])
      assert result == b(nand(:a, :b))
    end

    test "models user roles correctly" do
      constraint = Expression.at_most_one([:admin, :user, :guest])

      # Test scenario: user can be admin only
      assert Expression.run(constraint, &%{admin: true, user: false, guest: false}[&1]) == true

      # Test scenario: user cannot be both admin and user
      assert Expression.run(constraint, &%{admin: true, user: true, guest: false}[&1]) == false

      # Test scenario: nobody has a role (allowed with at_most_one)
      assert Expression.run(constraint, &%{admin: false, user: false, guest: false}[&1]) == true
    end
  end

  describe inspect(&Expression.all_or_none/1) do
    test "returns true for empty input" do
      assert Expression.all_or_none([]) == true
    end

    test "returns true for single variable" do
      assert Expression.all_or_none([:a]) == true
    end

    test "creates xnor constraint for two variables" do
      result = Expression.all_or_none([:a, :b])
      assert result == b(not (not (:a and :b) and (:a or :b)))
    end

    test "models feature flags correctly" do
      constraint = Expression.all_or_none([:dark_mode_ui, :dark_mode_api])

      # Both flags on together
      assert Expression.run(constraint, &%{dark_mode_ui: true, dark_mode_api: true}[&1]) == true

      # Both flags off together
      assert Expression.run(constraint, &%{dark_mode_ui: false, dark_mode_api: false}[&1]) == true

      # UI on but API off is invalid
      assert Expression.run(constraint, &%{dark_mode_ui: true, dark_mode_api: false}[&1]) == false
    end
  end

  describe inspect(&Expression.exactly_one/1) do
    test "returns false for empty input" do
      assert Expression.exactly_one([]) == false
    end

    test "returns the variable for single input" do
      assert Expression.exactly_one([:a]) == :a
    end

    test "creates constraints for exactly one of two variables" do
      result = Expression.exactly_one([:a, :b])
      # Should be: (at least one) AND (at most one)
      assert result == b(not (:a and :b) and (:a or :b))
    end

    test "models order status correctly" do
      constraint = Expression.exactly_one([:pending, :shipped])

      # Exactly pending is valid
      assert Expression.run(constraint, &%{pending: true, shipped: false}[&1]) == true

      # Exactly shipped is valid
      assert Expression.run(constraint, &%{pending: false, shipped: true}[&1]) == true

      # Both statuses is invalid
      assert Expression.run(constraint, &%{pending: true, shipped: true}[&1]) == false

      # No status is invalid
      assert Expression.run(constraint, &%{pending: false, shipped: false}[&1]) == false
    end
  end

  @spec var_as_fun(atom()) :: Macro.t()
  defp var_as_fun(var) do
    quote do
      unquote(var)()
    end
  end
end
