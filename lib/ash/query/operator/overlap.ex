# SPDX-FileCopyrightText: 2019 ash contributors <https://github.com/ash-project/ash/graphs.contributors>
#
# SPDX-License-Identifier: MIT

defmodule Ash.Query.Operator.Overlaps do
  @moduledoc """
  left overlaps [1, 2, 3]

  this predicate matches if the list on the left overlaps the list on the left
  """
  use Ash.Query.Operator,
    operator: :overlaps,
    predicate?: true,
    types: [[{:array, :same}, {:array, :same}]]

  @inspect_items_limit 10
  @dialyzer {:nowarn_function, compare: 2}

  def new(%Ash.Query.Ref{} = left, right) when is_list(right) do
    {:ok, %__MODULE__{left: left, right: MapSet.new(right)}}
  end

  def new(left, right), do: {:ok, %__MODULE__{left: left, right: right}}

  def can_return_nil?(%{left: left}), do: Ash.Expr.can_return_nil?(left)

  def evaluate(%{left: nil}), do: {:known, nil}
  def evaluate(%{right: nil}), do: {:known, nil}

  def evaluate(%{left: left, right: right}) do
    {:known, Enum.any?(left, fn l -> Enum.any?(right, &Comp.equal?(&1, l)) end)}
  end

  @impl Ash.Filter.Predicate
  def compare(
        %__MODULE__{left: left, right: %MapSet{} = mapset_left},
        %__MODULE__{left: left, right: %MapSet{} = mapset_right}
      ) do
    cond do
      MapSet.equal?(mapset_left, mapset_right) -> :mutually_inclusive
      MapSet.disjoint?(mapset_left, mapset_right) -> :mutually_exclusive
      true -> :unknown
    end
  end

  def compare(
        %__MODULE__{left: left, right: %MapSet{} = mapset},
        %Ash.Query.Operator.Has{left: left, right: value}
      ) do
    if MapSet.member?(mapset, value) do
      :left_includes_right
    else
      :mutually_exclusive
    end
  end

  def compare(_, _) do
    :unknown
  end

  def to_string(%{left: left, right: %MapSet{} = mapset}, opts) do
    import Inspect.Algebra

    list_doc =
      case Enum.split(mapset, @inspect_items_limit) do
        {left, []} -> to_doc(left, opts)
        {left, _} -> concat(to_doc(left, opts), "...")
      end

    concat([
      to_doc(left, opts),
      " overlaps ",
      list_doc
    ])
  end

  def to_string(op, opts), do: super(op, opts)
end
