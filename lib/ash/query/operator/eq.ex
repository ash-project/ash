defmodule Ash.Query.Operator.Eq do
  @moduledoc """
  left == right

  The simplest operator, matches if the left and right are equal.

  For comparison, this compares as mutually exclusive with other equality
  and `is_nil` checks that have the same reference on the left side
  """
  use Ash.Query.Operator,
    operator: :==,
    name: :eq,
    predicate?: true,
    types: [:same, :any]

  def evaluate(%{left: nil}), do: :unknown
  def evaluate(%{right: nil}), do: :unknown

  def evaluate(%{left: left, right: right}) do
    {:known, Comp.equal?(left, right)}
  end

  def bulk_compare(predicates) do
    predicates
    |> Enum.filter(&match?(%struct{} when struct == __MODULE__, &1))
    |> Enum.uniq()
    |> Enum.reject(fn
      %{right: %struct{}} when struct in [Ash.Query.Ref, Ash.Query.Call] ->
        true

      _ ->
        false
    end)
    |> Enum.group_by(& &1.left)
    |> Enum.flat_map(fn {_, predicates} ->
      Ash.SatSolver.mutually_exclusive(predicates)
    end)
  end
end
