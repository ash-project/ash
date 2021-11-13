defmodule Ash.Query.Operator.GreaterThan do
  @moduledoc """
  left > right

  In comparison, simplifies to `not(left < right + 1)`, so it will never need to be compared against.
  """
  use Ash.Query.Operator,
    operator: :>,
    name: :greater_than,
    predicate?: true,
    types: [:same, :any]

  def evaluate(%{left: left, right: right}) do
    {:known, Comp.greater_than?(left, right)}
  end

  def simplify(%__MODULE__{left: %Ref{} = ref, right: %Date{} = value}) do
    {:ok, op} = Ash.Query.Operator.new(Ash.Query.Operator.LessThan, ref, Date.add(value, 1))

    Ash.Query.Not.new(op)
  end

  def simplify(%__MODULE__{left: %Ref{} = ref, right: value}) when is_integer(value) do
    {:ok, op} = Ash.Query.Operator.new(Ash.Query.Operator.LessThan, ref, value + 1)

    Ash.Query.Not.new(op)
  end

  def simplify(_), do: nil
end
