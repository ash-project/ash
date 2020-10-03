defmodule Ash.Filter.Operator.IsNil do
  use Ash.Filter.Operator, operator: :is_nil

  def new(%Ref{} = left, right) when is_boolean(right) do
    {:ok, left, right}
  end

  def new(%Ref{}, right) do
    {:error, "#{inspect(right)} is not a valid operand for is_nil"}
  end

  def new(value, nil?), do: {:known, is_nil(value) == nil?}

  def match?(left, is_nil?) do
    is_nil(left) == is_nil?
  end

  def compare(%__MODULE__{left: %Ref{} = same_ref, right: true}, %Ash.Filter.Operator.Eq{
        left: %Ref{} = same_ref,
        right: nil
      }) do
    :mutually_inclusive
  end

  def compare(%__MODULE__{left: %Ref{} = same_ref, right: false}, %Ash.Filter.Operator.Eq{
        left: %Ref{} = same_ref,
        right: nil
      }) do
    :mutually_exclusive
  end

  def compare(%__MODULE__{left: %Ref{} = same_ref, right: false}, %Ash.Filter.Operator.Eq{
        left: %Ref{} = same_ref
      }) do
    :right_includes_left
  end

  def compare(%__MODULE__{left: %Ref{} = same_ref}, %__MODULE__{left: %Ref{} = same_ref}) do
    :mutually_exclusive
  end

  def compare(_, _), do: :unknown
end
