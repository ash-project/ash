# SPDX-FileCopyrightText: SPDX-FileCopyrightText: ash contributors <https://github.com/ash-project/ash/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.SatSolver do
  alias Crux.Expression
  require Crux.Expression

  @moduledoc deprectated: """
             This module is deprecated and will be removed in Ash 4.0.0.
             Please use `Crux` directly instead.
             """

  @doc deprecated: "Use Crux.b/1 instead"
  defmacro b(statement) do
    IO.warn("Deprecated, use Crux.b/1 instead", __CALLER__)

    Macro.prewalk(
      statement,
      fn
        {:and, _, [left, right]} ->
          quote do
            {:and, unquote(left), unquote(right)}
          end

        {:or, _, [left, right]} ->
          quote do
            {:or, unquote(left), unquote(right)}
          end

        {:not, _, [value]} ->
          quote do
            {:not, unquote(value)}
          end

        other ->
          other
      end
    )
  end

  @doc deprecated: "Use `Ash.Filter.strict_subset/2` instead."
  def strict_filter_subset(filter, candidate) do
    {:current_stacktrace, stacktrace} = Process.info(self(), :current_stacktrace)
    IO.warn("Use `Ash.Filter.strict_subset/2` instead.", stacktrace)

    Ash.Filter.strict_subset(filter, candidate)
  end

  @doc deprecated: "Use Ash.Expr.to_sat_expression/2 instead"
  def transform(resource, expression) do
    {:current_stacktrace, stacktrace} = Process.info(self(), :current_stacktrace)

    IO.warn(
      "Ash.SatSolver.transform/2 is deprecated, use Ash.Expr.to_sat_expression/2 instead",
      stacktrace
    )

    Ash.Expr.to_sat_expression(resource, expression)
  end

  @doc deprecated: """
       Use the following instead:

           Ash.Expr.to_sat_expression/2
           |> Crux.Formula.from_expression()
           |> Crux.solve()
       """
  def transform_and_solve(resource, expression) do
    {:current_stacktrace, stacktrace} = Process.info(self(), :current_stacktrace)

    IO.warn(
      """
      Ash.SatSolver.transform_and_solve/2 is deprecated. Use the following instead:

          Ash.Expr.to_sat_expression/2
          |> Crux.Formula.from_expression()
          |> Crux.solve()
      """,
      stacktrace
    )

    resource
    |> Ash.Expr.to_sat_expression(expression)
    |> Crux.Formula.from_expression()
    |> Crux.solve()
    |> case do
      {:error, :unsatisfiable} ->
        {:error, :unsatisfiable}

      {:ok, scenario} ->
        # Fake Indexes for old Format since this is a public function
        # Does not make a lot of sense because the indexes without the bindings
        # are not very meaningful...
        indices =
          scenario
          |> Enum.with_index(1)
          |> Enum.map(fn
            {{_variable, true}, index} -> index
            {{_variable, false}, index} -> 0 - index
          end)

        {:ok, indices}
    end
  end

  @doc deprecated: "Use `Ash.Resource.Info.synonymous_relationship_paths?/4` instead."
  def synonymous_relationship_paths?(
        left_resource,
        candidate,
        search,
        right_resource \\ nil
      ) do
    {:current_stacktrace, stacktrace} = Process.info(self(), :current_stacktrace)
    IO.warn("Use `Ash.Resource.Info.synonymous_relationship_paths?/4` instead.", stacktrace)

    Ash.Resource.Info.synonymous_relationship_paths?(
      left_resource,
      candidate,
      search,
      right_resource
    )
  end

  @doc deprecated: "Use `Crux.at_most_one/1` instead."
  def mutually_exclusive(predicates) do
    {:current_stacktrace, stacktrace} = Process.info(self(), :current_stacktrace)
    IO.warn("Use `Crux.at_most_one/1` instead.", stacktrace)

    case Expression.at_most_one(predicates) do
      result when result in [true, false] -> []
      result -> [result]
    end
  end

  @doc deprecated: "Use `Crux.exactly_one/1` instead."
  def mutually_exclusive_and_collectively_exhaustive(predicates) do
    {:current_stacktrace, stacktrace} = Process.info(self(), :current_stacktrace)
    IO.warn("Use `Crux.exactly_one/1` instead.", stacktrace)

    case Expression.exactly_one(predicates) do
      result when result in [true, false] -> []
      result -> [result]
    end
  end

  @doc deprecated: "Use `Crux.b(nand(left, right))` instead."
  def left_excludes_right(left, right) do
    {:current_stacktrace, stacktrace} = Process.info(self(), :current_stacktrace)
    IO.warn("Use `Crux.b(nand(left, right))` instead.", stacktrace)

    Expression.b(nand(left, right))
  end

  @doc deprecated: "Use `Crux.b(nand(left, right))` instead."
  def right_excludes_left(left, right) do
    {:current_stacktrace, stacktrace} = Process.info(self(), :current_stacktrace)
    IO.warn("Use `Crux.b(nand(left, right))` instead.", stacktrace)

    Expression.b(nand(left, right))
  end

  @doc deprecated: "Use `Crux.all_or_none/1` instead."
  def mutually_inclusive(predicates) do
    {:current_stacktrace, stacktrace} = Process.info(self(), :current_stacktrace)
    IO.warn("Use `Crux.all_or_none/1` instead.", stacktrace)

    case Expression.all_or_none(predicates) do
      result when result in [true, false] -> []
      result -> [result]
    end
  end

  @doc deprecated: "Use `Crux.b(implied_by(left, right))` instead."
  def right_implies_left(left, right) do
    {:current_stacktrace, stacktrace} = Process.info(self(), :current_stacktrace)
    IO.warn("Use `Crux.b(implied_by(left, right))` instead.", stacktrace)

    Expression.b(implied_by(left, right))
  end

  @doc deprecated: "Use `Crux.b(implies(left, right))` instead."
  def left_implies_right(left, right) do
    {:current_stacktrace, stacktrace} = Process.info(self(), :current_stacktrace)
    IO.warn("Use `Crux.b(implies(left, right))` instead.", stacktrace)

    Expression.b(implies(left, right))
  end

  @doc deprecated: "Use Crux.Formula.from_expression/1 instead"
  def to_cnf(expression) do
    %Crux.Formula{cnf: cnf, bindings: bindings} =
      Crux.Formula.from_expression(expression)

    current = bindings |> Map.keys() |> Enum.max(fn -> 0 end)

    old_bindings = Map.put(bindings, :current, current + 1)

    temp_bindings =
      1..current//1
      |> Map.new(&{&1, &1})
      |> then(
        &Map.merge(&1, %{
          reverse: &1,
          current: current
        })
      )

    {cnf,
     %{
       temp_bindings: temp_bindings,
       old_bindings: old_bindings
     }}
  end
end
