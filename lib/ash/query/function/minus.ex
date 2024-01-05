defmodule Ash.Query.Function.Minus do
  @moduledoc """
  Multiplies the value by negative one
  """
  use Ash.Query.Function, name: :-

  def args, do: [[:any]]

  def evaluate(%{arguments: [val]}) do
    {:ok, op} = Ash.Query.Operator.Basic.Times.new(val, -1)
    Ash.Query.Operator.evaluate(op)
  end
end
