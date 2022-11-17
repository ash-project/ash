list = Enum.to_list(1..10_000)
map_fun = fn i -> [i, i * i] end

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
      end) |> Ash.Policy.SatSolver.solve()
end

Benchee.run(
  %{
    solve: fn input ->
      Ash.Policy.SatSolver.solve(input)
    end
  },
  inputs: %{
      "3 conjunctive" => Enum.to_list(1..3) |> Enum.reduce(0, fn var, expr -> {:and, var, expr} end) |> Ash.Policy.SatSolver.solve(),
      "3 disjunctive" => Enum.to_list(1..3) |> Enum.reduce(0, fn var, expr -> {:or, var, expr} end) |> Ash.Policy.SatSolver.solve(),
      "3 mixed" => mixed.(3),
      "5 conjunctive" => Enum.to_list(1..5) |> Enum.reduce(0, fn var, expr -> {:and, var, expr} end) |> Ash.Policy.SatSolver.solve(),
      "5 disjunctive" => Enum.to_list(1..5) |> Enum.reduce(0, fn var, expr -> {:or, var, expr} end) |> Ash.Policy.SatSolver.solve(),
      "5 mixed" => mixed.(5),
      "7 conjunctive" => Enum.to_list(1..7) |> Enum.reduce(0, fn var, expr -> {:and, var, expr} end) |> Ash.Policy.SatSolver.solve(),
      "7 disjunctive" => Enum.to_list(1..7) |> Enum.reduce(0, fn var, expr -> {:or, var, expr} end) |> Ash.Policy.SatSolver.solve(),
      "7 mixed" => mixed.(7),
  }
)
