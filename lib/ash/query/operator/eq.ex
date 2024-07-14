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
    types: [:any, :same]

  def new(left, right) do
    {:ok, struct(__MODULE__, left: left, right: right)}
  end

  def evaluate(%{left: nil}), do: {:known, nil}
  def evaluate(%{right: nil}), do: {:known, nil}

  def evaluate(%{left: left, right: right}) do
    {:known, Comp.equal?(left, right)}
  end

  def can_return_nil?(%{left: left, right: right}),
    do: Ash.Expr.can_return_nil?(left) or Ash.Expr.can_return_nil?(right)

  @impl Ash.Filter.Predicate
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

  @impl Ash.Filter.Predicate
  def compare(
        %__MODULE__{left: left, right: left_right},
        %__MODULE__{left: left, right: right_right}
      ) do
    if Comp.equal?(left_right, right_right) do
      :mutually_inclusive
    else
      :mutually_exclusive
    end
  end

  def compare(%__MODULE__{} = left, %Ash.Query.Operator.In{} = right) do
    case Ash.Query.Operator.In.compare(right, left) do
      :left_includes_right -> :right_includes_left
      other -> other
    end
  end

  def compare(_, _), do: :unknown
end
