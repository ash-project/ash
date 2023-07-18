defmodule Ash.Query.Function.Minus do
  @moduledoc """
  Multiplies the value by negative one
  """
  use Ash.Query.Function, name: :-

  def args, do: [[:any]]

  def evaluate(%{arguments: [val]}) do
    {:known, -val}
  end
end
