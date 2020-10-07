defmodule Ash.Query.Operator.GreaterThan do
  @moduledoc """
  left > right

  In comparison, simplifies to `not(left < right + 1)`, so it will never need to be compared against.
  """
  use Ash.Query.Operator, operator: :>, predicate?: true

  def new(%Ref{attribute: %{type: type}} = left, right) do
    case Ash.Type.cast_input(type, right) do
      {:ok, casted} -> {:ok, left, casted}
      :error -> {:ok, left, right}
    end
  end

  def new(left, right) do
    {:known, left > right}
  end

  def evaluate(%{left: left, right: right}) do
    left > right
  end

  def simplify(%__MODULE__{left: %Ref{} = ref, right: value}) do
    {:ok, op} = Ash.Query.Operator.new(Ash.Query.Operator.LessThan, ref, value + 1)

    Ash.Query.Not.new(op)
  end

  def simplify(_), do: nil
end
