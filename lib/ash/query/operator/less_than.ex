defmodule Ash.Query.Operator.LessThan do
  @moduledoc """
  left < right

  Does not simplify, but is used as the simplification value for
  `Ash.Query.Operator.LessThanOrEqual`, `Ash.Query.Operator.GreaterThan` and
  `Ash.Query.Operator.GreaterThanOrEqual`.

  When comparing predicates, it is mutually exclusive with `Ash.Query.Operator.IsNil`.
  Additionally, it compares as mutually inclusive with any `Ash.Query.Operator.Eq` and
  any `Ash.Query.Operator.LessThan` who's right sides are less than it, and mutually
  exclusive with any `Ash.Query.Operator.Eq` or `Ash.Query.Operator.GreaterThan` who's
  right side's are greater than or equal to it.
  """

  use Ash.Query.Operator,
    operator: :<,
    name: :less_than,
    predicate?: true,
    types: [:same, :any]

  alias Ash.Query.Operator.{Eq, IsNil}

  def evaluate(%{left: nil}), do: {:known, nil}
  def evaluate(%{right: nil}), do: {:known, nil}

  def evaluate(%{left: left, right: right}) do
    {:known, Comp.less_than?(left, right)}
  end

  @impl Ash.Filter.Predicate
  def bulk_compare(all_predicates) do
    all_predicates
    |> Enum.group_by(& &1.left)
    |> Enum.flat_map(fn {_, all_predicates} ->
      predicates =
        all_predicates
        |> Enum.filter(&(&1.__struct__ in [__MODULE__, Eq]))
        |> Enum.sort_by(& &1.right)

      nil_exclusive(all_predicates) ++
        inclusive_values(predicates) ++ exclusive_values(predicates)
    end)
  end

  defp inclusive_values(sorted_predicates, acc \\ [])

  defp inclusive_values([], acc), do: acc

  defp inclusive_values([%Eq{} = first | rest], acc) do
    rest
    |> Enum.reject(&(&1.right == first.right))
    |> Enum.filter(&(&1.__struct__ == __MODULE__))
    |> case do
      [] ->
        inclusive_values(rest, acc)

      other ->
        new_acc =
          other
          |> Enum.map(&Ash.SatSolver.left_implies_right(first, &1))
          |> Kernel.++(acc)

        inclusive_values(rest, new_acc)
    end
  end

  defp inclusive_values([%__MODULE__{} = first | rest], acc) do
    rest
    |> Enum.reject(&(&1.right == first.right))
    |> Enum.filter(&(&1.__struct__ == Eq))
    |> case do
      [] ->
        inclusive_values(rest, acc)

      other ->
        new_acc =
          other
          |> Enum.map(&Ash.SatSolver.right_implies_left(first, &1))
          |> Kernel.++(acc)

        inclusive_values(rest, new_acc)
    end
  end

  defp exclusive_values(sorted_predicates, acc \\ [])
  defp exclusive_values([], acc), do: acc

  defp exclusive_values([%__MODULE__{} = first | rest], acc) do
    case Enum.filter(rest, &(&1.__struct__ == Eq)) do
      [] ->
        exclusive_values(rest, acc)

      other ->
        new_acc =
          other
          |> Enum.map(&Ash.SatSolver.left_excludes_right(first, &1))
          |> Kernel.++(acc)

        exclusive_values(rest, new_acc)
    end
  end

  defp exclusive_values([_ | rest], acc) do
    exclusive_values(rest, acc)
  end

  defp nil_exclusive(predicates) do
    is_nils = Enum.filter(predicates, &(&1.__struct__ == IsNil))

    case is_nils do
      [] ->
        []

      is_nils ->
        predicates
        |> Enum.filter(&(&1.__struct__ == __MODULE__))
        |> Enum.flat_map(fn lt ->
          Ash.SatSolver.mutually_exclusive([lt | is_nils])
        end)
    end
  end

  def can_return_nil?(%{left: left, right: right}),
    do: Ash.Expr.can_return_nil?(left) or Ash.Expr.can_return_nil?(right)
end
