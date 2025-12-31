# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Query.Function.IsNotDistinctFrom do
  @moduledoc """
  is_not_distinct_from(left, right)

  SQL's IS NOT DISTINCT FROM operator (NULL-safe equality).
  Unlike `==`, this operator treats NULL as equal to NULL.

  When both sides cannot return NULL, this simplifies to `==` for better performance.
  """
  use Ash.Query.Function, name: :is_not_distinct_from, predicate?: true

  def args, do: [[:any, :any]]

  def returns, do: [:boolean]

  def evaluate_nil_inputs?, do: true

  def new([left, right]) do
    if Ash.Expr.can_return_nil?(left) || Ash.Expr.can_return_nil?(right) do
      {:ok, struct(__MODULE__, arguments: [left, right])}
    else
      Ash.Query.Operator.new(Ash.Query.Operator.Eq, left, right)
    end
  end

  def evaluate(%{arguments: [left, right]}) do
    {:known, Comp.equal?(left, right)}
  end

  def can_return_nil?(_), do: false
end
