defmodule Ash.Query.Operator.NotEq do
  @moduledoc """
  left != right

  In comparison, simplifies to `not(left == right)`
  """
  use Ash.Query.Operator,
    operator: :!=,
    name: :not_eq,
    predicate?: true,
    types: [:same, :any]

  alias Ash.Query.Not
  alias Ash.Query.Operator.Eq

  def evaluate(%{left: nil}), do: {:known, nil}
  def evaluate(%{right: nil}), do: {:known, nil}

  def evaluate(%{left: left, right: right}) do
    {:known, Comp.not_equal?(left, right)}
  end

  @impl Ash.Filter.Predicate
  def simplify(%__MODULE__{left: left, right: right}) do
    %Not{expression: %Eq{left: left, right: right}}
  end

  def can_return_nil?(%{left: left, right: right}),
    do: Ash.Expr.can_return_nil?(left) or Ash.Expr.can_return_nil?(right)
end
