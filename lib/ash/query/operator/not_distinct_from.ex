# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Query.Operator.NotDistinctFrom do
  @moduledoc """
  left is_not_distinct_from right

  SQL's IS NOT DISTINCT FROM operator (NULL-safe equality).
  Unlike `==`, this operator treats NULL as equal to NULL.

  When both sides cannot return NULL, this simplifies to `==` for better performance.
  """
  use Ash.Query.Operator,
    operator: :is_not_distinct_from,
    name: :not_distinct_from,
    predicate?: true,
    types: [:same, :any]

  alias Ash.Query.Not
  alias Ash.Query.Operator.DistinctFrom

  @impl Ash.Query.Operator
  def evaluate(%{left: nil, right: nil}), do: {:known, true}
  def evaluate(%{left: nil, right: _}), do: {:known, false}
  def evaluate(%{left: _, right: nil}), do: {:known, false}

  def evaluate(%{left: left, right: right}) do
    {:known, Comp.equal?(left, right)}
  end

  @impl Ash.Query.Operator
  def evaluate_nil_inputs?, do: true

  @impl Ash.Filter.Predicate
  def simplify(%__MODULE__{left: left, right: right} = op) do
    if Ash.Expr.can_return_nil?(left) || Ash.Expr.can_return_nil?(right) do
      op
    else
      %Ash.Query.Operator.Equal{left: left, right: right}
    end
  end

  def can_return_nil?(_), do: false
end
