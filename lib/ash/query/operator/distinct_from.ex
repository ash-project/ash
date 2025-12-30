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
  def evaluate(%{left: nil, right: nil}), do: {:known, false}
  def evaluate(%{left: nil, right: _}), do: {:known, true}
  def evaluate(%{left: _, right: nil}), do: {:known, true}

  def evaluate(%{left: left, right: right}) do
    {:known, not Comp.equal?(left, right)}
  end

  @impl Ash.Query.Operator
  def evaluate_nil_inputs?, do: true

  @impl Ash.Filter.Predicate
  def simplify(%__MODULE__{left: left, right: right} = op) do
    if Ash.Expr.can_return_nil?(left) || Ash.Expr.can_return_nil?(right) do
      op
    else
      %Ash.Query.Operator.NotEqual{left: left, right: right}
    end
  end

  def can_return_nil?(_), do: false
end
