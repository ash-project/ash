# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Query.Function.Rem do
  @moduledoc """
  Rounds a float, decimal or integer to the given number of points
  """

  use Ash.Query.Function, name: :rem

  def args,
    do: [[:integer, :integer]]

  def returns, do: [:integer]

  def evaluate(%{arguments: [num1, num2]}) when is_integer(num1) and is_integer(num2),
    do: {:known, rem(num1, num2)}

  def can_return_nil?(_), do: false
end
