defmodule Ash.Filter.Operator.In do
  use Ash.Filter.Operator, operator: :in

  def new(%Ref{attribute: %{type: type}} = left, right) do
    case Ash.Type.cast_input({:array, type}, right) do
      {:ok, casted} -> {:ok, left, MapSet.new(casted)}
      :error -> {:ok, left, MapSet.new(right)}
    end
  end

  def new(left, right) do
    {:known, left in right}
  end

  def match?(left, right) do
    left in right
  end

  def compare(%__MODULE__{left: %Ref{} = same_ref, right: same_value}, %__MODULE__{
        left: %Ref{} = same_ref,
        right: same_value
      }) do
    :mutually_inclusive
  end

  def compare(%__MODULE__{left: %Ref{} = same_ref, right: left_values}, %__MODULE__{
        left: %Ref{} = same_ref,
        right: right_values
      }) do
    intersection = MapSet.intersection(left_values, right_values)
    count = MapSet.size(intersection)

    if count == 0 do
      :mutually_exclusive
    else
      intersection_pred =
        if count == 1 do
          Ash.Filter.Operator.new(Ash.Filter.Operator.Eq, same_ref, Enum.at(intersection, 0))
        else
          %__MODULE__{left: same_ref, right: intersection}
        end

      new_left = %__MODULE__{
        left: same_ref,
        right: MapSet.difference(left_values, intersection)
      }

      left_expr =
        Ash.Filter.Expression.new(
          :or,
          %__MODULE__{left: same_ref, right: new_left},
          intersection_pred
        )

      {:simplify, left_expr}
    end
  end

  def compare(%__MODULE__{left: %Ref{} = same_ref, right: left_values}, %Ash.Filter.Operator.Eq{
        left: %Ref{} = same_ref,
        right: right_value
      }) do
    if MapSet.member?(right_value, left_values) do
      :right_includes_left
    else
      :mutually_exclusive
    end
  end

  def compare(_, _), do: :unknown
end
