defmodule Ash.Filter.Operator.GreaterThan do
  @moduledoc """
  left > right

  In comparison, simplifies to `not(left < right + 1)`, so it will never need to be compared against.
  """
  use Ash.Filter.Operator, operator: :>

  def new(%Ref{attribute: %{type: type}} = left, right) do
    case Ash.Type.cast_input(type, right) do
      {:ok, casted} -> {:ok, left, casted}
      :error -> {:ok, left, right}
    end
  end

  def new(left, right) do
    {:known, left > right}
  end

  def match?(%{left: left, right: right}) do
    left > right
  end

  def simplify(%__MODULE__{left: %Ref{} = ref, right: value}) do
    {:ok, op} = Ash.Filter.Operator.new(Ash.Filter.Operator.LessThan, ref, value + 1)

    Ash.Filter.Not.new(op)
  end

  def simplify(_), do: nil
end
