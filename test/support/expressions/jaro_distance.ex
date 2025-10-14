# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Test.Expressions.JaroDistance do
  @moduledoc false
  use Ash.CustomExpression,
    name: :jaro_distance,
    arguments: [
      [:string, :string]
    ]

  def expression(data_layer, [left, right])
      when data_layer in [
             Ash.DataLayer.Ets,
             Ash.DataLayer.Simple
           ] do
    {:ok, expr(fragment(&__MODULE__.jaro_distance/2, ^left, ^right))}
  end

  def expression(_data_layer, _args), do: :unknown

  def jaro_distance(left, right) do
    String.jaro_distance(left, right)
  end
end
