defmodule Ash.Query.Operator.Has do
  @moduledoc """
  left has 1

  this predicate matches if the right is in the list on the left

  This actually just reverses the inputs and uses `in`.
  """
  use Ash.Query.Operator,
    operator: :has,
    predicate?: true,
    types: [[{:array, :any}, :same]]

  def new(left, right) do
    Ash.Query.Operator.In.new(right, left)
  end
end
