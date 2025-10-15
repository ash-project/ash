# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Query.Function.Intersects do
  @moduledoc """
  Returns true if the two arguments intersect.

     intersects([1, 2, 3], [1])
     true

     intersects([1, 2, 3], [4])
     false

     intersects([1], nil)
     nil

     intersects(nil, [1])
     nil
  """
  use Ash.Query.Function, name: :intersects, predicate?: true

  def args, do: [[{:array, :any}, {:array, :any}]]

  def returns, do: [:boolean]

  def evaluate(%{arguments: [nil, _]}), do: {:known, nil}
  def evaluate(%{arguments: [_, nil]}), do: {:known, nil}

  def evaluate(%{arguments: [right, left]}) when is_list(right) and is_list(left) do
    right = MapSet.new(right)
    left = MapSet.new(left)
    {:known, not MapSet.disjoint?(right, left)}
  end

  def evaluate(_other) do
    :unknown
  end

  def can_return_nil?(%{arguments: arguments}) do
    Enum.any?(arguments, &Ash.Expr.can_return_nil?/1)
  end
end
