defmodule Ash.Query.Operator.In do
  @moduledoc """
  left in [1, 2, 3]

  this predicate matches if the left is in the list on the right

  For comparison, this simplifies to a set of "or equals", e.g
  `{:or, {:or, {:or, left == 1}, left == 2}, left == 3}`
  """
  use Ash.Query.Operator, operator: :in, predicate?: true

  @inspect_items_limit 10

  def new(_, []), do: {:known, false}

  def new(%Ref{attribute: %{type: type}} = left, right) do
    case Ash.Type.cast_input({:array, type}, right) do
      {:ok, casted} -> {:ok, left, MapSet.new(casted)}
      :error -> {:ok, left, MapSet.new(right)}
    end
  end

  def new(left, right) when is_list(right) do
    {:known, left in right}
  end

  def evaluate(%{left: left, right: right}) do
    left in right
  end

  def to_string(%{right: %Ref{}} = op, opts), do: super(op, opts)

  def to_string(%{left: left, right: mapset}, opts) do
    import Inspect.Algebra

    list_doc =
      case Enum.split(mapset, @inspect_items_limit) do
        {left, []} -> to_doc(left, opts)
        {left, _} -> concat(to_doc(left, opts), "...")
      end

    concat([
      to_doc(left, opts),
      " in ",
      list_doc
    ])
  end

  def simplify(%__MODULE__{left: left, right: right}) do
    Enum.reduce(right, nil, fn item, expr ->
      {:ok, eq} = Ash.Query.Operator.new(Ash.Query.Operator.Eq, left, item)
      Ash.Query.Expression.new(:or, expr, eq)
    end)
  end
end
