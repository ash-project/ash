defmodule Ash.Constraints do
  def positive?(integer), do: integer >= 0
  def greater_than_zero?(integer), do: integer > 0
end
