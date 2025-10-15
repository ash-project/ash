# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Query.Function.Has do
  @moduledoc """
  Returns true if the second argument is found in the first

     contains([1, 2, 3], 1)
     true

     contains([1, 2, 3], 4)
     false

     contains(nil, 1)
     nil
  """
  use Ash.Query.Function, name: :has, predicate?: true

  def args, do: [[{:array, :any}, :any]]

  def returns, do: [:boolean]

  def evaluate(%{arguments: [nil, _]}), do: {:known, nil}

  def evaluate(%{arguments: [list, element]}) when is_list(list) do
    {:known, Enum.any?(list, &Comp.equal?(&1, element))}
  end

  def evaluate(_other) do
    :unknown
  end

  def can_return_nil?(%{arguments: [list, _]}) do
    Ash.Expr.can_return_nil?(list)
  end
end
