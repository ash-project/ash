# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Query.Operator.Has do
  @moduledoc """
  left has 1

  this predicate matches if the right value is in the list on the left
  """
  use Ash.Query.Operator,
    operator: :has,
    predicate?: true,
    types: [[{:array, :same}, :same]]

  @dialyzer {:nowarn_function, compare: 2}

  def new(left, right), do: {:ok, %__MODULE__{left: left, right: right}}

  def can_return_nil?(%{left: left}), do: Ash.Expr.can_return_nil?(left)

  def evaluate(%{left: nil}), do: {:known, nil}
  def evaluate(%{right: nil}), do: {:known, nil}

  def evaluate(%{left: left, right: right}) do
    {:known, Enum.any?(left, &Comp.equal?(&1, right))}
  end

  @impl Ash.Filter.Predicate
  def compare(
        %__MODULE__{left: left, right: left_right},
        %__MODULE__{left: left, right: right_right}
      ) do
    cond do
      left_right == right_right -> :mutually_inclusive
      left_right != right_right -> :mutually_exclusive
      true -> :unknown
    end
  end

  def compare(_, _) do
    :unknown
  end

  def to_string(op, opts), do: super(op, opts)
end
