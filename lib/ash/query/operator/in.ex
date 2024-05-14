defmodule Ash.Query.Operator.In do
  @moduledoc """
  left in [1, 2, 3]

  this predicate matches if the left is in the list on the right

  For comparison, this simplifies to a set of "or equals", e.g
  `{:or, {:or, {:or, left == 1}, left == 2}, left == 3}`
  """
  use Ash.Query.Operator,
    operator: :in,
    predicate?: true,
    types: [[:any, {:array, :same}]]

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
    {:known, Enum.any?(right, &Comp.equal?(&1, left))}
  end

  @impl Ash.Filter.Predicate
  def compare(%__MODULE__{left: left, right: %MapSet{} = left_right}, %__MODULE__{
        left: left,
        right: %MapSet{} = right_right
      }) do
    if MapSet.equal?(left_right, right_right) do
      :mutually_inclusive
    else
      if MapSet.disjoint?(left_right, right_right) do
        :mutually_exclusive
      else
        :unknown
      end
    end
  end

  def compare(%__MODULE__{}, %Ash.Query.Operator.Eq{
        right: %Ref{}
      }),
      do: false

  def compare(%__MODULE__{left: left, right: %MapSet{} = left_right}, %Ash.Query.Operator.Eq{
        left: left,
        right: value
      }) do
    if MapSet.member?(left_right, value) do
      :left_implies_right
    else
      :mutually_exclusive
    end
  end

  def compare(_, _), do: :unknown

  def to_string(%{left: left, right: %MapSet{} = mapset}, opts) do
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

  def to_string(op, opts), do: super(op, opts)
end
