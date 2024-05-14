defmodule Ash.Query.Operator.LessThanOrEqual do
  @moduledoc """
  left <= right

  In comparison, simplifies to `left < right + 1`, so it will never need to be compared against.
  """
  use Ash.Query.Operator,
    operator: :<=,
    name: :less_than_or_equal,
    predicate?: true,
    types: [:same, :any]

  def evaluate(%{left: nil}), do: {:known, nil}
  def evaluate(%{right: nil}), do: {:known, nil}

  def evaluate(%{left: left, right: right}) do
    {:known, Comp.less_or_equal?(left, right)}
  end

  @impl Ash.Filter.Predicate
  def simplify(%__MODULE__{left: %Ref{} = same_ref, right: %Date{} = value}) do
    {:ok, op} = Ash.Query.Operator.new(Ash.Query.Operator.LessThan, same_ref, Date.add(value, 1))

    op
  end

  def simplify(%__MODULE__{left: %Ref{} = same_ref, right: value}) when is_integer(value) do
    {:ok, op} = Ash.Query.Operator.new(Ash.Query.Operator.LessThan, same_ref, value + 1)

    op
  end

  def simplify(_), do: nil

  def can_return_nil?(%{left: left, right: right}),
    do: Ash.Expr.can_return_nil?(left) or Ash.Expr.can_return_nil?(right)
end
