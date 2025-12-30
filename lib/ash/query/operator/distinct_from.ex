# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Query.Operator.DistinctFrom do
  @moduledoc """
  left is_distinct_from right

  SQL's IS DISTINCT FROM operator.
  Unlike `!=`, this operator treats NULL as a comparable value.

  When both sides cannot return NULL, this simplifies to `!=` for better performance.
  """
  use Ash.Query.Operator,
    operator: :is_distinct_from,
    name: :distinct_from,
    predicate?: true,
    types: [:same, :any]

  @impl Ash.Query.Operator
  def new(left, right) do
    if Ash.Expr.can_return_nil?(left) || Ash.Expr.can_return_nil?(right) do
      {:ok, struct(__MODULE__, left: left, right: right)}
    else
      Ash.Query.Operator.new(Ash.Query.Operator.NotEq, left, right)
    end
  end

  @impl Ash.Query.Operator
  def evaluate(%{left: left, right: right}) do
    {:known, Comp.not_equal?(left, right)}
  end

  @impl Ash.Query.Operator
  def evaluate_nil_inputs?, do: true

  def can_return_nil?(_), do: false
end
