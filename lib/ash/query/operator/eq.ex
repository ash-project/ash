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

  def new(nil, right) when not is_nil(right) do
    Ash.Query.Operator.IsNil.new(right, true)
  end

  def new(left, nil) when not is_nil(left) do
    Ash.Query.Operator.IsNil.new(left, true)
  end

  def new(left, right) do
    {:ok, struct(__MODULE__, left: left, right: right)}
  end

  def evaluate(%{left: nil}), do: {:known, nil}
  def evaluate(%{right: nil}), do: {:known, nil}

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
