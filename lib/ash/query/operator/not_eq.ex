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

  def evaluate(%{left: nil}), do: {:ok, nil}
  def evaluate(%{right: nil}), do: {:ok, nil}

  def evaluate(%{left: left, right: right}) do
    {:known, Comp.not_equal?(left, right)}
  end

  def simplify(%__MODULE__{left: left, right: right}) do
    %Not{expression: %Eq{left: left, right: right}}
  end
end
