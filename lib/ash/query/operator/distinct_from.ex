# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Query.Operator.DistinctFrom do
  @moduledoc """
  left is_distinct_from right

  PostgreSQL's IS DISTINCT FROM operator.
  Unlike `!=`, this operator treats NULL as a comparable value:
  - NULL IS DISTINCT FROM NULL -> false
  - NULL IS DISTINCT FROM value -> true
  - value IS DISTINCT FROM NULL -> true
  - value IS DISTINCT FROM value -> false (if equal)
  - value IS DISTINCT FROM other_value -> true (if not equal)
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

  def can_return_nil?(_), do: false
end
