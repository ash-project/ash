defmodule Ash.Query.Function.Minus do
  @moduledoc """
  Multiplies the value by negative one
  """
  use Ash.Query.Function, name: :-

  def args, do: [[:any]]

  def returns, do: [:same]

  def evaluate(%{arguments: [val]}) do
    {:ok, op} = Ash.Query.Operator.Basic.Times.new(val, -1)
    Ash.Query.Operator.evaluate(op)
  end

  def can_return_nil?(%{arguments: [val]}) do
    Ash.Expr.can_return_nil?(val)
  end
end
