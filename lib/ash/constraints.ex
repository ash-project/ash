defmodule Ash.Constraints do
  @moduledoc false

  @spec positive?(integer) :: boolean
  def positive?(integer), do: integer >= 0

  @spec greater_than_zero?(integer) :: boolean
  def greater_than_zero?(integer), do: integer > 0
end
