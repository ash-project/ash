defmodule Ash.Query.Function.Round do
  @moduledoc """
  Rounds a float, decimal or integer to the given number of points
  """

  use Ash.Query.Function, name: :round

  def args,
    do: [
      [:float, :integer],
      [:decimal, :integer],
      [:integer, :integer],
      [:float],
      [:decimal],
      [:integer]
    ]

  def evaluate(%{arguments: [num]} = round), do: evaluate(%{round | arguments: [num, 0]})
  def evaluate(%{arguments: [num, _]}) when is_integer(num), do: {:known, num}

  def evaluate(%{arguments: [num, precision]}) when is_float(num),
    do: {:known, Float.round(num, precision)}

  def evaluate(%{arguments: [num, precision]}) do
    {:known, num |> Decimal.round(precision) |> Decimal.normalize()}
  end

  def can_return_nil?(%{arguments: [string]}) do
    Ash.Expr.can_return_nil?(string)
  end
end
