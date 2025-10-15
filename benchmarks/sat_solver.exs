# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs.contributors>
#
# SPDX-License-Identifier: MIT

mixed = fn count ->
  Enum.reduce(1..count, 0, fn var, expr ->
    cond do
      rem(var, 4) == 0 ->
        {:or, var, expr}

      rem(var, 3) == 0 ->
        {:and, expr, var}

      rem(var, 2) == 0 ->
        {:and, -var, expr}

      true ->
        {:or, -var, expr}
    end
  end)
end

Benchee.run(
  %{
    solve: fn input ->
      input
      |> Crux.Formula.from_expression()
      |> Crux.solve()
    end,
    satisfying_scenarios: fn input ->
      input
      |> Crux.Formula.from_expression()
      |> Crux.satisfying_scenarios()
    end,
    decision_tree: fn input ->
      input
      |> Crux.Formula.from_expression()
      |> Crux.decision_tree()
    end
  },
  inputs: %{
    "3 conjunctive" => Enum.reduce(1..3, 0, fn var, expr -> {:and, var, expr} end),
    "3 disjunctive" => Enum.reduce(1..3, 0, fn var, expr -> {:or, var, expr} end),
    "3 mixed" => mixed.(3),
    "5 conjunctive" => Enum.reduce(1..5, 0, fn var, expr -> {:and, var, expr} end),
    "5 disjunctive" => Enum.reduce(1..5, 0, fn var, expr -> {:or, var, expr} end),
    "5 mixed" => mixed.(5),
    "7 conjunctive" => Enum.reduce(1..7, 0, fn var, expr -> {:and, var, expr} end),
    "7 disjunctive" => Enum.reduce(1..7, 0, fn var, expr -> {:or, var, expr} end),
    "7 mixed" => mixed.(7)
  }
)
