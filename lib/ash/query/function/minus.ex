# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Query.Function.Minus do
  @moduledoc """
  Negates the value
  """
  use Ash.Query.Function, name: :-

  def args, do: [[:any]]

  def returns, do: [:same]

  def evaluate(%{arguments: [duration]}) when is_struct(duration, Duration) do
    {:known, Duration.negate(duration)}
  end

  def evaluate(%{arguments: [val]}) do
    {:ok, op} = Ash.Query.Operator.Basic.Times.new(val, -1)
    Ash.Query.Operator.evaluate(op)
  end

  def can_return_nil?(%{arguments: [val]}) do
    Ash.Expr.can_return_nil?(val)
  end
end
