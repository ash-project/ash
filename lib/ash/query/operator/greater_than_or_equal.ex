defmodule Ash.Query.Operator.GreaterThanOrEqual do
  @moduledoc """
  left >= right

  In comparison, simplifies to `not(left < right)`, so it will never need to be compared against.
  """
  use Ash.Query.Operator,
    operator: :>=,
    name: :greater_than_or_equal,
    predicate?: true,
    types: [:same, :any]

  def evaluate(%{left: nil}), do: {:known, nil}
  def evaluate(%{right: nil}), do: {:known, nil}

  def evaluate(%{left: left, right: right}),
    do: {:known, Comp.greater_or_equal?(left, right)}

  def can_return_nil?(%{left: left, right: right}),
    do: Ash.Expr.can_return_nil?(left) or Ash.Expr.can_return_nil?(right)

  @impl Ash.Filter.Predicate
  def simplify(%__MODULE__{left: %Ref{} = ref, right: value}) do
    {:ok, op} = Ash.Query.Operator.new(Ash.Query.Operator.LessThan, ref, value)

    Ash.Query.Not.new(op)
  end

  def simplify(_), do: nil
end
