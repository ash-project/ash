defmodule Ash.Filter.Operator.Eq do
  @moduledoc """
  left == right

  The simplest operator, matches if the left and right are equal.

  For comparison, this compares as mutually exclusive with other equality
  and `is_nil` checks that have the same reference on the left side
  """
  use Ash.Filter.Operator, operator: :==

  def new(%Ref{} = ref, nil) do
    Ash.Filter.Operator.new(Ash.Filter.Operator.IsNil, ref, true)
  end

  def new(%Ref{attribute: %{type: type}} = left, right) do
    case Ash.Type.cast_input(type, right) do
      {:ok, casted} ->
        {:ok, left, casted}

      _ ->
        {:error,
         Ash.Error.Query.InvalidFilterValue.exception(
           value: right,
           message: "Could not be casted to type #{inspect(type)}",
           context: %__MODULE__{left: left, right: right}
         )}
    end
  end

  def new(left, right) do
    {:known, left == right}
  end

  def match?(%{left: left, right: right}) do
    left == right
  end

  def bulk_compare(predicates) do
    predicates
    |> Enum.filter(&Kernel.match?(%struct{} when struct in [__MODULE__, IsNil], &1))
    |> Enum.group_by(& &1.left)
    |> Enum.uniq()
    |> Enum.flat_map(fn {_, predicates} ->
      Ash.SatSolver.mutually_exclusive(predicates)
    end)
  end
end
