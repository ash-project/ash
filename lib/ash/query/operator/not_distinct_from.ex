# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Query.Operator.NotDistinctFrom do
  @moduledoc """
  left is_not_distinct_from right

  PostgreSQL's IS NOT DISTINCT FROM operator (NULL-safe equality).
  In comparison, simplifies to `not(left is_distinct_from right)`
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
  def simplify(%__MODULE__{left: left, right: right}) do
    %Not{expression: %DistinctFrom{left: left, right: right}}
  end

  def can_return_nil?(_), do: false
end
