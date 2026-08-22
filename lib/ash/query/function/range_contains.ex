# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs/contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Query.Function.RangeContains do
  @moduledoc """
  Returns true if a range holds a value, which may be a point or another range.

  An unbounded end holds everything beyond it, and an empty range holds no point. A
  bound holds its own value only when it is inclusive.

     range_contains(range, value)
  """
  use Ash.Query.Function, name: :range_contains, predicate?: true

  alias Ash.Range

  def args, do: [[:any, :any]]

  def returns, do: [:boolean]

  def evaluate(%{arguments: [nil, _]}), do: {:known, nil}
  def evaluate(%{arguments: [_, nil]}), do: {:known, nil}

  def evaluate(%{arguments: [%Range{} = range, value]}) do
    {:known, Range.contains?(range, value)}
  end

  def evaluate(_other), do: :unknown

  def can_return_nil?(%{arguments: arguments}) do
    Enum.any?(arguments, &Ash.Expr.can_return_nil?/1)
  end
end
