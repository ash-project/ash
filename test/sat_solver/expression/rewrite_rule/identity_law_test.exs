# SPDX-FileCopyrightText: 2020 Zach Daniel
#
# SPDX-License-Identifier: MIT

# credo:disable-for-this-file Credo.Check.Warning.BoolOperationOnSameValues
defmodule Ash.Test.SatSolver.Expression.RewriteRule.IdentityLaw do
  use ExUnit.Case, async: true
  use ExUnitProperties

  import Ash.SatSolver.Expression, only: [b: 1]

  alias Ash.SatSolver.Expression
  alias Ash.SatSolver.Expression.RewriteRule.IdentityLaw

  doctest IdentityLaw, import: true

  describe inspect(&IdentityLaw.walk/1) do
    test "applies AND identity" do
      assert Expression.postwalk(b(:a and true), &IdentityLaw.walk/1) == :a
      assert Expression.postwalk(b(true and :a), &IdentityLaw.walk/1) == :a
    end

    test "applies OR identity" do
      assert Expression.postwalk(b(:a or false), &IdentityLaw.walk/1) == :a
      assert Expression.postwalk(b(false or :a), &IdentityLaw.walk/1) == :a
    end

    test "leaves non-matching patterns unchanged" do
      assert Expression.postwalk(b(:a and :b), &IdentityLaw.walk/1) == b(:a and :b)
      assert Expression.postwalk(b(:a or :b), &IdentityLaw.walk/1) == b(:a or :b)
      assert Expression.postwalk(:a, &IdentityLaw.walk/1) == :a
    end
  end

  property "identity law preserves logical equivalence" do
    check all(
            assignments <-
              StreamData.map_of(StreamData.atom(:alphanumeric), StreamData.boolean(),
                min_length: 1
              ),
            variable_names = Map.keys(assignments),
            expr <- Expression.generate_expression(StreamData.member_of(variable_names))
          ) do
      result = Expression.postwalk(expr, &IdentityLaw.walk/1)
      eval_fn = &Map.fetch!(assignments, &1)

      assert Expression.run(expr, eval_fn) == Expression.run(result, eval_fn),
             """
             Identity law changed the logical outcome!
             Original: #{inspect(expr, pretty: true)}
             Transformed: #{inspect(result, pretty: true)}
             Assignments: #{inspect(assignments, pretty: true)}
             """
    end
  end
end
